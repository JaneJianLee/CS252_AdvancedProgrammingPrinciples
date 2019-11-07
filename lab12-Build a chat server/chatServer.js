var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {
      
    //Q1) Typing '\list' will list the names of all other users.
    if(data.toString() === '\\list\r\n'){
        list(client);
    }
    //Q2) Typing '\rename <newname>' will let the user specify a new name for himself/herself.
    else if(data.toString().slice(0,7) === '\\rename'){
        rename(data, client);
    }
    //Q3) Typing '\private <name> <msg>' will send a message only to the specified user.
    else if(data.toString().slice(0,8) === '\\private'){
        privatechat(data, client);
    }
    else{ 
        broadcast(data, client);
    }
  });
});

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name + " says " + data+eol);
    }
  }
}

//'\list'
function list(client){
  client.write(" # Currently Connected users are :  ");
  for (var i in clientList) {
    if (client !== clientList[i]) {
        client.write(clientList[i].name+"  ");
    }
    else{
        client.write(clientList[i].name+"(YOU)"+"  ");
    }
  }
  client.write(eol);
}

//'\rename <newname>'
function rename(data, client){
    client.write(" # Renaming client name.. \r\n"+eol);
    var newname = data.toString().trim().split(' ');
    client.name = newname[1];
    client.write(" # Your new name is : " + client.name +"\r\n"+eol);
}

//'\private <name> <msg>'
function privatechat(data, client){
    client.write(" # Sending private message..  \r\n"+eol);
    var temp = data.toString().trim().split(' ');
    var receiver = temp[1];
    var namelen = receiver.length+8;
    var msg = data.toString().slice(namelen+2);
    for (var i in clientList) {
        if (receiver == clientList[i].name) {
            clientList[i].write("[Private message] "+ client.name + " sends " + msg+eol);
        }
    }
}


srvr.listen(9000);
