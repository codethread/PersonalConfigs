#!/bin/bash

mkdir $1
cd $1
git init
npm init -y
echo "module.exports = {
extends: ['airbnb', 'prettier'],
rules: {
'func-names': 'off',
'prettier/prettier': 'error'
},
plugins: ['prettier'],
env: {
browser: true,
node: true,
jest: true
},
settings: {}
}" > .eslintrc.js
echo "node_modules" > .gitignore
echo "var a = chuff" > app.js

npx install-peerdeps --dev eslint-config-airbnb
yarn add --dev prettier eslint-plugin-prettier eslint-config-prettier

vim app.js
