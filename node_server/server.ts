// Node.js micro service to provide a network interface for the Haskell back end.

const readline = require("readline");
const express = require("express");

type CommandObj = {
  command : string,
  arguments : string[]
};

function forwardCommand(comm : CommandObj) : Promise<string> {
  let commandLine = comm.command;
  comm.arguments.forEach((arg) => {commandLine += ` ${arg}`});
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  console.log(commandLine);
  const commandResolution = new Promise<string>((resolve) => {
    rl.on("line", (input) => {
      resolve(input);
    });
  });
  return commandResolution;
}

function main() : void {
  const app = express();
  const port = 80;
  const staticDir = "C:\\Users\\steve\\code\\GD_server_deployed\\";

  app.use(express.static(staticDir));
  app.use(express.json());

  app.post("/command", async (req, res) => {
    const result = await forwardCommand(req.body);
    res.status(200).send(result).end();
  });

  app.listen(port, () => {

  });
}

main();

