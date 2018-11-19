/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2018-
 *
 * Most things in this file are copied from the GCC[1] souce code,
 * and are only slightly modified to fit our needs.
 *
 * [1] https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libiberty/argv.c;hb=HEAD
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "fs_rts.h"

#include <string.h>

#if defined(HAVE_CTYPE_H)
#include <ctype.h>
#endif

/**
Note [Processing response files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While parsing command-line arguments, 'setupRtsFlags' keeps an eye out for
arguments that begin with the character '@'. Such an argument is interepreted
as a 'response file'. The contents of the response file are interpreted as
additional command-line options. In particular, the file is separated into
whitespace-separated strings; each such string is taken as a command line
option. The new options are inserted in place of the option naming the response
file, and *argc and *argv will be updated.

For example, consider this program invocation:

    ./foo a1 a2 @more_options a3

Where 'more_options' is a file which contains 3 options 'b1 b2 b3'.
'expandargv' updates 'argv' as if the program had been called with:

    ./foo a1 a2 b1 b2 b3 a3
*/

char** dupargv (char * const *argv);
char **buildargv (const char *input);
void expandargv (uint32_t *argcp, char ***argvp, uint32_t *arg_i);

#define INITIAL_MAXARGC 8 /* Number of args + NULL in initial argv */

#if !defined(EOS)
#define EOS '\0'
#endif

static void consume_whitespace (const char **input) {
    while (isspace (**input)) {
        (*input)++;
    }
}

static int only_whitespace (const char* input) {
    while (*input != EOS && isspace (*input))
        input++;

    return (*input == EOS);
}

/*
Duplicate an argument vector.  Simply scans through the vector,
duplicating each argument until the terminating NULL is found.
Returns a pointer to the argument vector if successful.  Returns
NULL if there is insufficient memory to complete building the
argument vector.
*/
char ** dupargv (char * const *argv) {
    int argc;
    char **copy;

    if (argv == NULL)
        return NULL;

    // the vector
    for (argc = 0; argv[argc] != NULL; argc++);
    copy = (char **) stgMallocBytes ((argc + 1) * sizeof (char *), "dupargv");

    // the strings
    for (argc = 0; argv[argc] != NULL; argc++)
        copy[argc] = strdup (argv[argc]);
    copy[argc] = NULL;
    return copy;
}

/*
Given a pointer to a string, parse the string extracting fields
separated by whitespace and optionally enclosed within either single
or double quotes (which are stripped off), and build a vector of
pointers to copies of the string for each field.  The input string
remains unchanged.  The last element of the vector is followed by a
NULL element.
All of the memory for the pointer array and copies of the string
is obtained from stgMallocBytes.
Returns a pointer to the argument vector if successful.  Returns
NULL if 'input' is NULL or if there is insufficient
memory to complete building the argument vector.
If the input is a null string (as opposed to a NULL pointer),
then buildarg returns an argument vector that has one arg, a null
string.
The memory for the argv array is dynamically expanded as necessary.
In order to provide a working buffer for extracting arguments into,
with appropriate stripping of quotes and translation of backslash
sequences, we allocate a working buffer at least as long as the input
string.  This ensures that we always have enough space in which to
work, since the extracted arg is never larger than the input string.
*/
char **buildargv (const char *input) {
    char *arg;
    char *copybuf;
    int squote = 0;
    int dquote = 0;
    int bsquote = 0;
    int argc = 0;
    int maxargc = 0;
    char **argv = NULL;
    char **nargv;

    if (input != NULL) {
        copybuf = (char *) stgMallocBytes (strlen (input) + 1, "buildargv");
        // Is a do{}while to always execute the loop once.  Always return an
        // argv, even for null strings.  See NOTES above, test case below.
        do {
            // Pick off argv[argc]
            consume_whitespace (&input);

            if ((maxargc == 0) || (argc >= (maxargc - 1))) {
                // argv needs initialization, or expansion
                if (argv == NULL) {
                    maxargc = INITIAL_MAXARGC;
                    nargv = (char **) stgMallocBytes (maxargc * sizeof (char *),
                                                      "buildargv");
                }
                else {
                    maxargc *= 2;
                    nargv = (char **) realloc (argv, maxargc * sizeof (char *));
                }
                argv = nargv;
                argv[argc] = NULL;
            }
            // Begin scanning arg
            arg = copybuf;
            while (*input != EOS) {
                if (isspace (*input) && !squote && !dquote && !bsquote) {
                    break;
                }
                else {
                    if (bsquote) {
                        bsquote = 0;
                        *arg++ = *input;
                    }
                    else if (*input == '\\') {
                        bsquote = 1;
                    }
                    else if (squote) {
                        if (*input == '\'') {
                            squote = 0;
                        } else {
                            *arg++ = *input;
                        }
                    }
                    else if (dquote) {
                        if (*input == '"') {
                            dquote = 0;
                        } else {
                            *arg++ = *input;
                        }
                    }
                    else {
                        if (*input == '\'') {
                            squote = 1;
                        }
                        else if (*input == '"') {
                            dquote = 1;
                        }
                        else {
                            *arg++ = *input;
                        }
                    }
                    input++;
                }
            }
            *arg = EOS;
            argv[argc] = strdup (copybuf);
            argc++;
            argv[argc] = NULL;

            consume_whitespace (&input);
        }
        while (*input != EOS);

        free (copybuf);
    }
    return (argv);
}

// See Note [Processing response files]
//
void expandargv (uint32_t *argcp, char ***argvp, uint32_t *arg_i) {
    // To check if ***argvp has been dynamically allocated.
    char ** const original_argv = *argvp;

    // The name of the response file.
    const char *filename;

    // The response file.
    FILE *f;

    // An upper bound on the number of characters in the response file.
    long pos;

    // The number of characters in the response file, when actually read.
    size_t len;

    // A dynamically allocated buffer used to hold options read from a
    // response file.
    char *buffer;

    // Dynamically allocated storage for the options read from the
    // response file.
    char **file_argv;

    // The number of options read from the response file, if any.
    size_t file_argc;

    // The +1 skips the '@' before the file name.
    filename = (*argvp)[*arg_i] + 1;

    // Read the contents of the file.
    f = fopen (filename, "r");
    if (!f) {
        sysErrorBelch("Warning: Could not open response file %s", filename);
        return;
    }
    if (fseek (f, 0L, SEEK_END) == -1)
        goto error;
    pos = ftell (f);
    if (pos == -1)
        goto error;
    if (fseek (f, 0L, SEEK_SET) == -1)
        goto error;

    buffer = (char *) stgMallocBytes (pos * sizeof (char) + 1, "expandargv");
    len = fread (buffer, sizeof (char), pos, f);
    if (len != (size_t) pos
        // On Windows, fread() may return a value smaller than POS,
        // due to CR/LF->CR translation when reading text files.
        // That does not in-and-of itself indicate failure.
        && ferror (f))
        goto error;

    // Add a NULL terminator.
    buffer[len] = EOS;

    // If the file is empty or contains only whitespace, buildargv would
    // return a single empty argument.  In this context we want no arguments,
    // instead.
    if (only_whitespace (buffer)) {
        file_argv = (char **) stgMallocBytes (sizeof (char *), "expandargv");
        file_argv[0] = NULL;
    } else {
        // Parse the string.
        file_argv = buildargv (buffer);
    }

    // If *ARGVP is not already dynamically allocated, copy it.
    if (*argvp == original_argv)
        *argvp = dupargv (*argvp);

    // Count the number of arguments.
    file_argc = 0;
    while (file_argv[file_argc])
        ++file_argc;

    // Free the original option's memory.
    free ((*argvp)[*arg_i]);

    // Now, insert FILE_ARGV into ARGV.  The "+1" below handles the
    // NULL terminator at the end of ARGV.
    *argvp = (char **)
             stgReallocBytes (*argvp,
                              (*argcp + file_argc + 1) * sizeof (char *),
                              "expandargv");

    memmove (*argvp + *arg_i + file_argc, *argvp + *arg_i + 1,
             (*argcp - *arg_i) * sizeof (char *));
    memcpy (*argvp + *arg_i, file_argv, file_argc * sizeof (char *));

    // The original option has been replaced by all the new options.
    *argcp += file_argc - 1;

    // (*argvp)[*arg_i] has been updated to contain the first flag from the
    // response file. We must now set things up so that this flag is
    // processed by 'setupRtsFlags'.
    (*arg_i)--;

    // Free up memory allocated to process the response file.
    free (file_argv);
    free (buffer);

    // We're all done with the file now.
    fclose (f);
    return;

error:
    sysErrorBelch("Warning: Could not process response file %s", filename);
    fclose (f);
}
