import * as fs from "fs";
import * as parser from "./parser"

function main() {
    if (process.argv.length !== 3) {
        return console.error("Please specify a story file.");
    }
    let filePath = process.argv[2];
    var result = parser.story.parse(fs.readFileSync(filePath).toString());
    console.dir(result);
}
main();
