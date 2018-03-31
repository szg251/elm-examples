const path                  = require('path');
const CleanWebpackPlugin    = require('clean-webpack-plugin');
const HtmlWebpackPlugin     = require('html-webpack-plugin');

const cleanPlugin    = new CleanWebpackPlugin('dist');
const htmlPlugin     = new HtmlWebpackPlugin({
    title : 'Todo App',
    template : 'src/index.html'
});

module.exports = (env, argv) => ({
    module : {
        rules : [
            {
                test    : /\.elm$/,
                exclude : [
                    path.join(__dirname, 'src/elm-stuff'),
                    path.join(__dirname, 'node_modules'),
                ],
                use : {
                    loader: 'elm-webpack-loader',
                    options: {
                        debug: argv.mode === 'development',
                    },
                },
            },
            {
                test : /\.css$/,
                use  : ['style-loader', 'css-loader' ],
            },
        ],
    },

    plugins: argv.mode === 'production'
        ? [cleanPlugin, htmlPlugin]
        : [htmlPlugin]
    ,

    devServer : {
        port : 8000,
    },
});
