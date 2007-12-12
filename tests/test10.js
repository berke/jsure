function toto(b) {
  with(this) {
    var options = select.getElementsByTagName("option");
    assertEqual("choice1", options[0].value);
    assertEqual("other", options[1].value);
    assert( options[1].selected, "second option is selected"); 
    assertEqual("other", select.getValue());

    select.addOption("choice3", "third", true);
    assert( options[1].selected, "second option is still selected (multiple=true)"); 
    assert( options[2].selected, "third option is also selected"); 
    var options = select.getElementsByTagName("option");
    if(x) {
      return false;
    } else {
      return false;
    }
    return foo;
  }
}

