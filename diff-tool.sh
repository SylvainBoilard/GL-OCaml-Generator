#!/bin/sh

cd $(dirname $(realpath $0))

BUILD_DIR=_build/default
REFS_DIR=_refs

print_help ()
{
    echo "Available options :"
    echo "  -d|--diff       Print the differences between generated and reference files."
    echo "                  This is the default if no option is used. The output can be"
    echo "                  used to write a patch file."
    echo "  -p|--promote    Promote generated files. If no files are currently tracked,"
    echo "                  start tracking a set of default files."
    echo "  -P              Same as -p, but does not ask for confirmation."
    echo "  -h|--help       Print this message."
}

promote_files ()
{
    FILES="${*:-GL.ml GL_stubs.c gl.xml}"
    for f in $FILES; do
        [ -f "$BUILD_DIR/$f" ] || continue
        mkdir -p $(dirname "$REFS_DIR/$f")
        cp --remove-destination "$BUILD_DIR/$f" "$REFS_DIR/$f"
    done
}

if [ $# -gt 1 ]; then
    echo Too many arguments.
    print_help
    exit 1
fi

FILES=$([ -d "$REFS_DIR" ] && find "$REFS_DIR" -type f -printf "%P ")

case "$1" in
    "-d"|"--diff"|"")
        [ -z "$FILES" ] && { echo No files to diff. ; print_help ; exit 1 ; }
        [ -t 1 ] && COLOR="always" || COLOR="never"
        for f in $FILES; do
            diff -U3 --color=$COLOR "$REFS_DIR/$f" --label "a/$f" "$BUILD_DIR/$f" --label "b/$f"
        done | less -R -F
        ;;
    "-p"|"--promote")
        read -p "Really promote files? [y/N] " REALLY
        case "$REALLY" in
            [yY]|[yY]es|YES)
                promote_files $FILES
                echo Promoted.
                ;;
            *)
                echo Canceled.
                exit 1
        esac
        ;;
    "-P")
        promote_files $FILES
        ;;
    "-h"|"--help")
        print_help
        ;;
    *)
        echo Unknown option "$1".
        print_help
        exit 1
esac
