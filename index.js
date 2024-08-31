document.addEventListener('DOMContentLoaded', function() {
    var app = Elm.Main.init({
        node: document.getElementById('root')
    });

    app.ports.copyToClipboard.subscribe(function(text) {
        navigator.clipboard.writeText(text).then(function() {
            console.log('Copying to clipboard was successful!');
        }, function(err) {
            console.error('Could not copy text: ', err);
        });
    });
});