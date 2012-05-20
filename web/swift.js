function check_swift(swift, res, err)
{
  var been_called = false;
  post_to_url('swift_checker.scm',
              function(x)
                {
                  if (x.length != 0 && !been_called)
                    {d = x; // debug
been_called = true; res(JSON.parse(x));}
                },
              JSON.stringify({swift: swift}),
              err);
}

var swift_res_ids = ["res_note",
                     "res_country_code", "res_country_name",
                     "res_bank_id", "res_branch_id"];

function clear_swift_results()
{
  swift_res_ids.forEach(function(id) { element_set_by_id(id, ""); });
}

function click_check_swift()
{
  alert("durr");
  var swift = document.getElementById("swift").value;
  check_swift(
    swift,
    function(x)
      {
        clear_swift_results();
      });
}
