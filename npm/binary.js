const { Binary } = require('binary-install');
const os = require('os');
const { version, name, repository } = require("../package.json");

function getPlatform() {
    const type = os.type();

    if (type === 'Windows_NT') return 'Windows';
    if (type === 'Linux') return 'Linux';
    if (type === 'Darwin') return 'macOS';

    throw new Error(`Unsupported platform: ${type}`);
}

function getBinary() {
    const platform = getPlatform();
    const url = `${repository.url}/releases/download/v${version}/breeze-${version}-${platform}.tar.gz`;
    return new Binary(name, url);
}

module.exports = getBinary;
