os=$(uname -s)
arch=$(uname -m)
mkdir -p "./release"
stack install --local-bin-path "./release"
mv "./release/decdns" "./release/decdns-$os-$arch"
