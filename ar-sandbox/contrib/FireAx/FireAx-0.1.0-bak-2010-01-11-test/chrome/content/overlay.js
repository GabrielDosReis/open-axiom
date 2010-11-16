var FireAx = {
  onLoad: function() {
    // initialization code
    this.initialized = true;
  },

  onMenuItemCommand: function() {
    window.open("chrome://FireAx/content/FireAx.xul", "", "chrome");
  }
};

window.addEventListener("load", function(e) { FireAx.onLoad(e); }, false); 
