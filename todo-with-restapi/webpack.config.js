const path                  = require('path');
const CleanWebpackPlugin    = require('clean-webpack-plugin');
const HtmlWebpackPlugin     = require('html-webpack-plugin');
const UglifyJSWebpackPlugin = require('uglifyjs-webpack-plugin');

const cleanPlugin    = new CleanWebpackPlugin('dist');
const htmlPlugin     = new HtmlWebpackPlugin({
    title : 'Todo App',
    template : 'src/index.html'
});
const uglifyjsPlugin = new UglifyJSWebpackPlugin();

const isProduction = process.env.NODE_ENV === 'production';

module.exports = {
    entry : {
        app : './src/index.js',
    },

    output : {
        path : path.join(__dirname, 'dist'),
        filename : '[name].js',
    },

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
                        debug: !isProduction,
                    },
                },
            },
            {
                test : /\.css$/,
                use  : ['style-loader', 'css-loader' ],
            },
        ],

        noParse : /\.elm$/,
    },

    plugins: isProduction
        ? [cleanPlugin, htmlPlugin, uglifyjsPlugin]
        : [htmlPlugin]
    ,

    devServer : {
        port : 8000,
    },
}
