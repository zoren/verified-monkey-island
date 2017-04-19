module.exports = {
    entry: "./entry.ts",
    output: {
        path: __dirname + "/dist",
        filename: "run.bundle.js",
        sourceMapFilename: "run.bundle.map",
        // export itself to a global var
        libraryTarget: "var",
        // name of the global var: "Foo"
        library: "Run"
    },
    resolve: {
        // Add '.ts' and '.tsx' as a resolvable extension.
        extensions: [".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".js.map"]
    },
    module: {
        loaders: [
            // all files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'
            { test: /\.ts$/, loader: "ts-loader" }
        ]
    }
}
