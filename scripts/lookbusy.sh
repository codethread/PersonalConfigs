cd /Users/adh23/PR/sky-pages

while true
do
 rm -rf ./node_modules
 npm cache clear
 npm i
 npm run test:unit || true
done
