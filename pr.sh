cp $1 "./processor/$2"
cd processor
sed -i '$ d' $2
cargo run --release > $3