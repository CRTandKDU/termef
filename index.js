const express	= require('express');
const app	= express();

const http	= require('http');
const server	= http.createServer(app);

const { Server } = require( 'socket.io' );
const io = new Server( server );

var ucount = 0;

app.get('/', (req, res) => {
    res.sendFile(__dirname + '/index.html');
});

io.on( 'connection', (socket) => {
    ucount += 1;
    console.log( "User %d connected", ucount );
    socket.on( 'disconnect', () => {
	console.log( "User %d disconnected", ucount );
	ucount -= 1;
    });
    socket.on( 'update', (msg) => {
	console.log( "From %d : %s", ucount, msg );
	// msg is JSON
	io.emit( 'update', msg );
    });
});

server.listen( 3000, () => {
    console.log('Server listening on *:3000');
});
