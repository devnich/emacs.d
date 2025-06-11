# See `install_instructions.org` in this directory for instructions on how to
# use this script.

for orig_name in *.dylib
do
    if [[ ! "$orig_name" == libtree-sitter* ]];
    then
        new_name="libtree-sitter-${orig_name}"
        echo $new_name
        mv "$orig_name" "$new_name"
    else
        echo "$orig_name already exists"
    fi
done
