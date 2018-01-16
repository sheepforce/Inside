function reloadGraph() {
   var now = new Date();

   document.images['coldion'].src = 'coldion.png?' + now.getTime();
   document.images['lakeshorea'].src = 'lakeshorea.png?' + now.getTime();
   document.images['lakeshoreb'].src = 'lakeshoreb.png?' + now.getTime();
   document.images['graphixthree1a'].src = 'graphixthree1a.png?' + now.getTime();
   document.images['graphixthree1b'].src = 'graphixthree1b.png?' + now.getTime();
   document.images['graphixthree1c'].src = 'graphixthree1c.png?' + now.getTime();
   document.images['graphixthree2a'].src = 'graphixthree2a.png?' + now.getTime();
   document.images['graphixthree2b'].src = 'graphixthree2b.png?' + now.getTime();
   document.images['graphixthree2c'].src = 'graphixthree2c.png?' + now.getTime();


   // Start new timer (1 sec)
   timeoutID = setTimeout('reloadGraph()', 3000);
}
