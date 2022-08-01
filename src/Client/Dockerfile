########## Global Arguments ############

ARG ELM_VERSION=0.19
ARG ELM_SOURCE=ui
ARG ELM_ROOT=/home/ui
ARG ELM_TARGET_FOLDER=$ELM_ROOT/dist
ARG ELM_TARGET_FILE=$ELM_TARGET_FOLDER/assets/application.js
ARG BULMA_SOURCE=$ELM_SOURCE/mybulma
ARG BULMA_ROOT=/home/bulma

ARG RELEASE_HOME=/usr/share/nginx/html

############ Elm Build  ################

FROM codesimple/elm:$ELM_VERSION AS elm-build

ARG ELM_VERSION
ARG ELM_SOURCE
ARG ELM_ROOT
ARG ELM_TARGET_FOLDER
ARG ELM_TARGET_FILE

WORKDIR $ELM_ROOT
COPY $ELM_SOURCE .
RUN elm make src/Main.elm --output $ELM_TARGET_FILE --optimize

WORKDIR $BULMA_ROOT
COPY $BULMA_SOURCE .
RUN npm install --legacy-peer-deps && npm run build


############ Elm Runtime  ################
FROM nginx:alpine as base

ARG RELEASE_HOME
ARG ELM_TARGET_FOLDER

WORKDIR $RELEASE_HOME
COPY --from=elm-build $ELM_TARGET_FOLDER $RELEASE_HOME/

