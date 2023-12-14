const { io }		= require("socket.io-client");
const epc		= require( 'elrpc' );
var elparser		= require('elparser');
const miserables	= {
    nodes: [
	{ id: "apprentissage antagoniste", group: 1},
	{ id: "apprentissage autosupervis�", group: 1},
	{ id: "apprentissage par transfert", group: 1},
	{ id: "grand mod�le de langage", group: 1},
	{ id: "g�n�ration automatique d'image", group: 1},
	{ id: "g�n�ration automatique de texte", group: 1},
	{ id: "instruction g�n�rative", group: 1},
	{ id: "intelligence artificielle g�n�rative", group: 1},
	{ id: "jeton textuel", group: 1},
	{ id: "mode loquace", group: 1},
	{ id: "mod�le d'apprentissage pr�entra�n�", group: 1},
	{ id: "mod�le g�n�ratif", group: 1},
	{ id: "mod�le � diffusion de bruit statistique", group: 1},
	{ id: "notice de mod�le d'apprentissage", group: 1},
	{ id: "transformeur", group: 1}
    ]
    ,
    links: [
	{source:"mod�le � diffusion de bruit statistique", target:"mod�le g�n�ratif", value:1},
	{source:"mod�le � diffusion de bruit statistique", target:"instruction g�n�rative", value:1},
	{source:"mod�le � diffusion de bruit statistique", target:"g�n�ration automatique d'image", value:1},
	{source:"grand mod�le de langage", target:"g�n�ration automatique de texte", value:1},
	{source:"apprentissage antagoniste", target:"mod�le d'apprentissage pr�entra�n�", value:1},
	{source:"transformeur", target:"apprentissage autosupervis�", value:1},
	{source:"apprentissage par transfert", target:"mod�le d'apprentissage pr�entra�n�", value:1},
	{source:"intelligence artificielle g�n�rative", target:"mod�le g�n�ratif", value:1},
	{source:"mod�le d'apprentissage pr�entra�n�", target:"apprentissage par transfert", value:1},
	{source:"notice de mod�le d'apprentissage", target:"mod�le d'apprentissage pr�entra�n�", value:1},
	{source:"instruction g�n�rative", target:"grand mod�le de langage", value:1},
	{source:"mod�le g�n�ratif", target:"mod�le d'apprentissage pr�entra�n�", value:1},
	{source:"mod�le g�n�ratif", target:"transformeur", value:1},
	{source:"g�n�ration automatique d'image", target:"mod�le � diffusion de bruit statistique", value:1},
	{source:"g�n�ration automatique de texte", target:"grand mod�le de langage", value:1},
	{source:"g�n�ration automatique de texte", target:"instruction g�n�rative", value:1},
	{source:"jeton textuel", target:"instruction g�n�rative", value:1},
	{source:"jeton textuel", target:"grand mod�le de langage", value:1}]
};


const socket = io( "ws://localhost:3000" );

socket.on( 'error', (err) => {
    console.log( err );
});


epc.startServer().then( function(server) {
    server.defineMethod( "echo", function(args) {
	console.log( String(args) );
	// socket.emit( 'update', JSON.stringify( miserables ) );
	socket.emit( 'update', String(args) );
	return args;
    });
    
    server.wait();
});


