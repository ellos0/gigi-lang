web:
    pug -o web/src/index.pug web/out
userbuild:
    #NEVER use this in dev
    #It will delete the web folder
    rm -r web
build:
    gcc source/interpreter.c -o source/interpreter

