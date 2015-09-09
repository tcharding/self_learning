#!/bin/bash
#
# newchapter - set up directory for start of new chapter
#
# # Coding conventions
#
# * globals are `like_this`.
# * locals are `_like_this`.
# * exported values are `LIKE_THIS`.
#
# Tobin Harding

set -u # Undefined variables are errors

main() {
    assert_cmds
    set_globals
    handle_command_line_args "$@"
    print_init_message
    make_new_dir
    if [ $templates = true ]; then
	say "Copying templates:"
	copy_templates
    fi
}

set_globals () {
    debug="on"
    script="$(basename "$0")"    

    # Environment sanity checks
    assert_nz "$HOME" "\$HOME is undefined"
    assert_nz "$0" "\$0 is undefined"

    # Some constants
    version=0.0.1

    # Command line arguments
    dir=

    # option 
    templates=true
}

make_new_dir() {
    ensure mkdir "$dir"
    ensure rm -rf cur			# remove soft link
    ln -s "$dir" cur
}

copy_templates () {
    local _file=

    _file="${dir}/Makefile"
    ensure cp Makefile.template "$_file"
    echo -n "    "
    echo "$_file"

    _file="${dir}/ANSWERS.md"
    ensure cp ANSWERS.template.md "$_file"
    echo -n "    "
    echo "$_file"

}
    
debug () {
    if [ debug == "on" ]; then say "$1"; fi
}

handle_command_line_args() {

    # man gotopt(1)
    local _temp="$(getopt -o vh --long no-templates,help,version -n "$script" -- "$@")"
    if (( $? )); then
	err "getopt error"
    fi
    eval set -- "$_temp"
    
    # handle options
    while true; do
	case "$1" in
	    -h | --help ) print_help ; exit 0 ;;
	    -v | --version ) echo "$script $version$" ; exit 0 ;;
	    --no-templates ) templates=false; shift ;;
	    -- ) shift; break ;;
	    * ) break ;;
	esac
    done

    # handle arguments
    dir="$1"
}

print_init_message() {
    cat <<EOF
$script: Creating new chapter directory:
    $dir

EOF
}

# Help

print_help() {
echo '
Usage: newchapter.sh dir_name

Options:
    --no-templates   Create directory without copying templates
'
}

# Standard utilities

say() {
    echo "$script: $1"
}

say_err() {
    say "$1" >&2
}

verbose_say() {
    if [ "$flag_verbose" = true ]; then
	say "$1"
    fi
}

err() {
    say "$1" >&2
    exit 1
}

need_cmd() {
    if ! command -v "$1" > /dev/null 2>&1
    then err "need '$1' (command not found)"
    fi
}

need_ok() {
    if [ $? != 0 ]; then err "$1"; fi
}

assert_nz() {
    if [ -z "$1" ]; then err "assert_nz $2"; fi
}

# Run a command that should never fail. If the command fails execution
# will immediately terminate with an error showing the failing
# command.
ensure() {
    "$@"
    need_ok "command failed: $*"
}

# This is just for indicating that commands' results are being
# intentionally ignored. Usually, because it's being executed
# as part of error handling.
ignore() {
    run "$@"
}

# Runs a command and prints it to stderr if it fails.
run() {
    "$@"
    local _retval=$?
    if [ $_retval != 0 ]; then
	say_err "command failed: $*"
    fi
    return $_retval
}

# Prints the absolute path of a directory to stdout
abs_path() {
    local _path="$1"
    # Unset CDPATH because it causes havok: it makes the destination unpredictable
    # and triggers 'cd' to print the path to stdout. Route `cd`'s output to /dev/null
    # for good measure.
    (unset CDPATH && cd "$_path" > /dev/null && pwd)
}

assert_cmds() {
    need_cmd getopt
    need_cmd mkdir
    need_cmd cp
}

main "$@"
