# Mirror of the source code of zymake

- See [main site](http://www-personal.umich.edu/~ebreck/code/zymake/) for more information.

----
# A copy of the original webpage follows below, for documentation purposes
----

`zymake` is a high-level language for running complex sets of
experiments. The user writes a zymakefile, mostly consisting of
parameterized shell commands, and zymake determines the dependency
structure and executes the commands in the appropriate order.

Highlights
----------

-   make-like semantics, shell-like syntax
-   execution order follows dependencies, files are rebuilt only if
    older than files on which they depend, etc.
-   all filenames are inferred by the system
-   a file is determined by a set of key-value pairs, such as
    "method=svm" or "number-of-hidden-units=10"
-   simple interpolation syntax: everything apart from whitespace and
    \$(...) is passed untouched to the shell for execution.
-   parallel execution

Links
-----

-   [A slideshow (PDF) introducing
    `zymake`](/~ebreck/data/zymake/talk.pdf)
-   [Another slideshow (based on the paper
    below)](/~ebreck/data/zymake/AclSofteng.pdf)
-   [Download a Windows version of
    zymake](/~ebreck/data/zymake/win/zymake.exe)
-   [Download a Linux-x86 version of
    zymake](/~ebreck/data/zymake/linux/zymake)
-   [Download a Mac OSX-PPC version of
    zymake](/~ebreck/data/zymake/ppc/zymake)
-   [Download the zymake source
    code](/~ebreck/data/zymake/zymake-0.3.0-src.zip). Requires an
    installation of [Objective Caml](http://caml.inria.fr) to compile.
-   [Read a paper](http://www.aclweb.org/anthology/W/W08/W08-0503.pdf)
    -- `2008` Eric Breck. **zymake: a computational workflow
    system for machine learning and natural language processing.** In
    *Proceedings of the Workshop on 2008 ACL workshop on Software
    Engineering, Testing, and Quality Assurance for Natural Language
    Processing*.
-   A complete real-world [zymakefile](/~ebreck/data/zymake/ijcai07-run)
    used for the experiments reported in (Breck, Choi & Cardie, '07).
-   [An announcement-only mailing list for users interested in
    `zymake`.](http://tech.groups.yahoo.com/group/zymake-announce/)

Try it! - download zymake for your platform and the zymakefile above,
and run

`zymake -d ijcai07-run`

to see what it would execute (nothing will actually be run, that's what
`-d` means)

Some introductory examples
--------------------------

-   **Compilation**

    The make rule

        %.exe: %.c
                cc -o $@ $^

    would be written in zymake as

        cc -o $(>).exe $().c

    Note that rather than separating the specification of dependencies
    from the shell command, zymake integrates the two. In this case, the
    output file (the .exe) needs to be specified with the \> character.
    The semantics of this rule are almost identical to that of make.

-   **Cross-validation**

    Suppose we have two commands. `run` takes an argument (the
    cross-validation fold) and produces an output (the result of running
    on that fold). `average-folds` takes a list of
    `run`-outputs and averages them, producing a LaTeX table. We
    can run this for 10 folds like this.

        run $(fold) > $().eval

        average_folds $(fold=*(range 1 10)) $().eval > $().table

Usage
-----

A `zymake` file consists of a series of *definitions* and
*rules*. A definition defines an immutable global variable. A rule
specifies a shell command to run, which takes certain kinds of files as
input and certain files as output. Rules with no outputs are called
*queries*, and the goal is to be able to execute all of the queries.
What `zymake` does is to determine what commands are necessary to
be able to execute the queries, and in what order to execute them.

For those who care, this involves constructing a directed acyclic graph
(in which each node is an interpolated rule) and executing each node in
topological order.

Parallel execution
------------------

This part is not fully tested. It seems to work, but I haven't done
anything large-scale with it yet. The basic idea is that as you're
proceeding through the dag in topological order, at any point where
multiple rules could be executed next, you execute them all in parallel.

There are, at the moment, two methods of parallel execution provided.
First, the user provide a list of compute nodes in a special global
variable called `machines`, and the system works out which nodes
are least loaded, and runs the processes on those using `ssh`.
Second, the user can provide a script `start` which will run a
given command on another machine (presumably through some sort of
job-submission). `zymake` requires that this script wait until
the job completes beore returning.

I'm open to other sorts of interfaces here, e.g. with the machine
learning cluster's queueing mechanisms, I just don't know what they are.

Files (key-value sets)
----------------------

One of the things that differentiates `zymake` from standard
`make` is how it understands files. Essentially, `make`
treats each filename as a string. For `zymake`, a file is a set
of key-value pairs. For example, a file might be defined by
`model=svm fold=2 C=0.5`. Each file also has a distinguished key,
the file suffix (such as `.svm`, `.eval`, or
`.output`). Each file does not have to define a value for each
key.

Matching
--------

A rule need only specify the information about a file that is relevant
for that rule. Other keys will be inferred and added as necessary. For
example, the rule for an evaluation script might specify

	```bash
    eval $(metric) $().predictions > $().eval
	```

The `.eval` file must have the `metric` key, but it may
have many other keys as well, which will be passed along to the
`.predictions` file if they are needed.

Starting with the queries, the matcher tries to figure out how to build
each file needed. It matches the files it needs against all the rules,
trying to find a rule which produces an output all of whose keys are
present in the needed file. This must match exactly one rule; matching
zero or more than one rule is an error.

Syntax
------

As much as possible, the goal of `zymake`'s syntax is to avoid
having unnecessary escaping. Therefore, the rules that you write in the
zymakefile correspond to the strings that are passed to the shell for
execution, with two exceptions: any sequence of whitespace (including
newlines) are collapsed into a single space, and interpolations -
anything beginning with the characters \$( and ending with a matching )
- are replaced by their value. Different rules are separated by blank
lines.

The syntax for global variable definitions is like that of rules, except
that a definition begins with `identifier =`. Definitions can
also appear on adjacent lines without an intervening blank line.

### Comments

Any line beginning with `#` is a comment. Any comments
immediately preceding a rule (with no intervening blank lines) are
associated with the rule. During execution, the comment can optionally
be displayed when the rule is executed, providing the user with a
description of what's going on that may be more comprehensible than the
command string.

A real example:

    grep -v "fold $(fold)" $().nz-svm | perl -ane '$eos = /#endsent/; 
    $class = m{#in'$(class)' }?"pos":"neg"; s/#.*//; @F=split; 
    print "@F[1..$#F] $class\n"; print "\n" if $eos' >$(both="false").mallet-train

What does this do? Well, if I precede it with the comment:

    # Make training file

then at runtime, 'Make training file' can be printed in addition to (or
instead of) the command-string above.

Interpolations
--------------

There are two kinds of interpolations: expression interpolations, and
file interpolations. An expression interpolation computes some
expression whose value is interpolated into the command to be executed.
A file interpolation represents an input or output file created or
needed by the rule. The file's name is inferred, and the filename is
interpolated into the command.

Syntax:

File-interpolation ::= `$(` `key1` `=`
`value1` `key2` `=` `value2` ... `)`
.suffix

Expression-interpolation ::= `$(` `value` `)`

Expression-interpolation ::= `$(` `value0` `value1`
`value2` ... `)`

The latter syntax is shorthand for

`$( ( value0 value1 value2 .... ) )`, i.e. it saves you a level
of parentheses.

Value ::= integer-literal | string-literal | identifier | `(`
`value0` `value1` `value2` .... `)`

Identifiers evaluate to the value of the corresponding key, or the value
of the corresponding global variable if no key exists.

Lists evaluate by evaluating the first value. If this is a special form,
it is directly applied to the later values; otherwise, the other values
are evaluated, and the function is applied to them.

Interpolations are always introduced by the characters `$(`. To
interpolate the literal characters `$(`, write `$(()`.

File interpolations are currently not allowed in global variable
definitions. It's not clear what it would mean; if you can come up with
a compelling semantics and use case for this, let me know and I'll think
about including it.

### Input and output files

A file interpolation is an input unless otherwise specified. If the
interpolation begins with the `>` character, it is an output, or
if the most recent character before the interpolation was a `>`
(to cover the common case of creating a file by output redirection).
This latter case can be overridden by beginning the interpolation with
`<`.

### "splats"

There are often cases where you'd like to write a single expression but
have it represent a list of objects. For example, I might want to write
that a final table depends on evaluation outputs from 10
cross-validation folds. I could write this using a 'splat':

     make-table $(fold=*(1 2 3 4 5 6 7 8 9 10)).eval > $().table 

The asterisk indicates that the file interpolation should be replicated
once for each value in the list following the asterisk.

More than one key can be 'splatted' in a given file interpolation, in
which case the cross-product of all values will be created (i.e.
splatting a 3-value list and a 4-value list will result in 12 files).

You can also splat expression interpolations by writing
`$( * value)`. This allows you to join existing lists together.

### Functions

A small set of functions and special forms is provided for use in
interpolations.

-   `quote` Like Lisp or Scheme, this prevents evaluation of the
    following expression. This can also be written by preceding the
    expression with a single quote '
-   `list` creates a list composed of the following expressions.
-   `flatten` creates a list from a list of lists
-   `range` creates a list from a start value to a finish value,
    either of integers or of characters
-   `split` takes a string and returns a list of strings,
    splitting the initial string by whitespace
-   `concat` takes a list of strings or integers and returns the
    result of joining them all together (with no intervening spaces)
-   `shell` takes a string argument, and returns the standard
    output of executing that command. This is just like the shell
    function in GNU make or backticks in shells or Perl.
-   ... there are some others

Filenames
---------

To zymake, a file is uniquely determined by its set of key-value pairs
(including the suffix). The filesystem, however, requires that a file
have a string name. Therefore, zymake has a way of creating a mapping
between filenames and key-value sets. One of the basic principles of
zymake is that the user shouldn't depend on what these filenames look
like (apart from the suffix). But since you probably will want to look
at the files individually, here's a guide to how zymake does the
mapping.

First, zymake creates a mapping from 'labels' to key, value pairs. If
the value only occurs with one key in your zymakefile, then the label
will often just be the value (with some modification to make it a
filesystem-friendly string). If the value is a variable interpolation,
the label will often be the name of the variable. Additional characters
may be added to the end of the label to make this mapping unique. This
mapping is written out to the file `o/o.zymakefilename._dict`.

Next, zymake creates a name for a file by concatenating the labels for
all its key, value pairs (in a fixed order), followed by the suffix,
separated by periods. Finally, zymake prefixes a string unique to the
zymakefile, typically `o/o.zymakefilename.`

Key set inference
-----------------

`FIXME: replace this with declarative description`

The goal is that a file should contain all and only the keys it needs.
The algorithm to determine which keys a file has is this:

-   start at the queries (commands with no output), with empty keys.
-   execute (command, keys):
-   propagate all keys to inputs, adding any explicit input keys.
-   for each input execute (input,keys); replace input with this return
    val.
-   output gets keys filtered by: keys used by inputs, keys mentioned in
    interpolations in this command, keys mentioned in output.
-   return output

Query mode
----------

`zymake.byt -q QUERY zymakefile` prints to standard output the
result of parsing QUERY as if it were a command in a zymakefile. You can
use this, among other things, to print out the filename that zymake
would create for a given file interpolation: e.g.
`zymake.byt -q '$(a=1 b="foo").bar' zymakefile` would print out
something like "o/zymakefile/o.1.foo.bar".

Future features
---------------

-   Additional facilities for running jobs in parallel
-   Run-time variance of the dag
-   ...

Reporting bugs
--------------

Send me an e-mail if something breaks. The more info the better. In
particular, you can run:

    zymake -vvv zymakefile

and send me everything that gets spewed out. If it's so much spew that
it's taking forever, delete some of the -vs.

Contributors
------------

`zymake` was written by [Eric
Breck](http://www-personal.umich.edu/~ebreck/).\

