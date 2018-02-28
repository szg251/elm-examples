const path = require('path');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    entry: {
        app: './src/index.js',
    },

    output: {
        path: path.join(__dirname, 'dist'),
        filename: '[name].js',
    },

    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [
                    path.join(__dirname, 'src/elm-stuff'),
                    path.join(__dirname, 'node_modules'),
                ],
                use: {
                    loader: 'elm-webpack-loader',
                }
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader' ]
            },
        ],

        noParse: /\.elm$/,
    },

    plugins: [
        new CleanWebpackPlugin('dist'),
        new HtmlWebpackPlugin({
            title: 'Todo App',
            template: 'src/index.html',
        }),
    ],

    devServer: {
        port: 8000,
        inline: true,
        stats: { colors: true },
        contentBase: path.join(__dirname, 'dist'),
    }

}
