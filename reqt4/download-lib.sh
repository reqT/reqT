wget -O tmp.zip https://github.com/lampepfl/dotty/releases/download/3.1.1-RC1/scala3-3.1.1-RC1.zip
unzip tmp.zip -d tmp
mkdir -p lib
cp tmp/scala3*/lib/*.jar lib/. 
rm -rf tmp
rm tmp.zip