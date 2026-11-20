#!/bin/sh

cd $(dirname $(realpath $0))

BUILD_DIR=_build/default
REFS_DIR=_refs

print_help ()
{
    echo "Available options :"
    echo "  -d|--diff       Output the diff between generated files and originals."
    echo "  -p|--promote    Promote generated files."
    echo "  -P              Promote generated files without confirmation."
    echo "  -h|--help       Print this message."
}

promote_files ()
{
    for f in "$*"; do
        [ -f "$BUILD_DIR/$f" ] || continue
        mkdir -p $(dirname "$REFS_DIR/$f")
        cp "$BUILD_DIR/$f" "$REFS_DIR/$f"
    done
}

if [ $# -gt 1 ]; then
    echo Too many arguments.
    print_help
    exit 1
fi

FILES=$(find "$REFS_DIR" -type f -printf "%P ")

case "$1" in
    "-d"|"--diff"|"")
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
