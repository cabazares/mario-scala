FROM node:10-alpine

WORKDIR /app

COPY package.json ./
COPY yarn.lock ./

RUN yarn

COPY . .

EXPOSE 3000

ARG WS_ADDRESS='ws://localhost:8080'
ENV REACT_APP_WS_ADDRESS $WS_ADDRESS

CMD [ "yarn", "run", "start" ]
