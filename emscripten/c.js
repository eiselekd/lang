var Module = {
    onRuntimeInitialized: function() {
        Module.callJsBackWithAgrs();
    }
};

function jsMethodAgrs(title, msg) {
    alert(title + '\n' + msg);
}
