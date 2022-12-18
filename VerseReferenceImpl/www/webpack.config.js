const path = require('path');
const webpack = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
    mode: 'development',
    externals: {
        'wasmer_wasi_js_bg.wasm': true
    },
    experiments: {
        asyncWebAssembly: true,
        topLevelAwait: true
    },
    plugins: [
        new webpack.ProvidePlugin({
            Buffer: ['buffer', 'Buffer']
        }),
        new CopyPlugin({
            patterns: [
                {
                    from: 'static'
                }
            ]
        })
    ],
    resolve: {
        fallback: {
            buffer: require.resolve('buffer')
        }
    },
    devServer: {
        client: {
            overlay: false
        },
        static: {
            directory: path.join(__dirname, "dist")
        },
        headers: {
            "Access-Control-Allow-Origin": "*",
            "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
            "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization"
        }
    }
};
