FROM nginx:alpine as base

RUN apk add --update npm nodejs
RUN npm install -g elm-spa
RUN npm --prefix ./mybulma install mybulma --legacy-peer-deps

FROM base as build
WORKDIR /usr/src/app
COPY . .
RUN elm-spa build 
RUN npm --prefix ./mybulma run build ./mybulma

FROM build as deploy
COPY --from=build /usr/src/app/public /usr/share/nginx/html/

