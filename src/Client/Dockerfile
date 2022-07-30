FROM nginx:alpine as base

RUN apk add --update npm nodejs
RUN npm install -g elm-spa
WORKDIR mybulma
RUN npm install --legacy-peer-deps

FROM base as build
WORKDIR /usr/src/app
COPY . .
RUN elm-spa build 
WORKDIR mybulma
RUN npm run build

FROM build as deploy
COPY --from=build /usr/src/app/public /usr/share/nginx/html/

