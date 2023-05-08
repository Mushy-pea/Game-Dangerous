function postRequest(data : string, url : string) : Promise<number> {
    const http = require("http");
    const options = {
      hostname: "127.0.0.1",
      port: 80,
      path: url,
      method: "POST",
      headers: {
          "Content-Type": "application/json",
          "Content-Length": data.length
      }
    };
    let req;
    const postResolution = new Promise<number>((resolve, reject) => {
      req = http.request(options, res => {
        res.pipe(process.stdout);
  
        res.on("end", () => {
          resolve(res.statusCode);
        });
  
        res.on("error", error => {
          console.error(error);
          reject(null);
        });
      });
    });
  
    req.write(data);
    req.end();
    return postResolution;
  }
  
async function main() : Promise<void> {
  const command = {
    command: "read",
    arguments: [
      "0", "0", "0", "7", "7", "Wall_grid"
    ]
  };
  const commandStr = JSON.stringify(command);
  const testURL = "/command";
  const result = await postRequest(commandStr, testURL);
  console.log(`result: ${result}`);
}

main();

