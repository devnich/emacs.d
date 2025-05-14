for orig_name in *.dylib
do
    new_name="libtree-sitter-${orig_name}"
    echo $new_name
    mv "$orig_name" "$new_name"
done
