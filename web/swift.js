function check_swift(swift, res, err)
{
  var been_called = false;
  post_to_url('swift_checker.scm',
              function(x)
                {
                  if (x.length != 0 && !been_called)
                    { been_called = true; res(JSON.parse(x));}
                },
              JSON.stringify({swift: swift}),
              err);
}

function clear_swift_results()
{
  var results = fetch_element_by_id("results");
  while (results.children.length > 0)
    results.removeChild(results.children[0]);
}

function display_swift_results(res)
{
  var parent = fetch_element_by_id("results");
  parent.appendChild(
    element_from_string("<p>Country: " + res['country-name'] + "</p>"));
  if (res.details['bank-name'])
    parent.appendChild(
      element_from_string(
        "<p>Bank name: " + res.details['bank-name'] + "</p>"));
  if (res.details.branch)
    parent.appendChild(
      element_from_string(
        "<p>Branch: " + res.details.branch + "</p>"));
  if (res.details.address)
    parent.appendChild(
      element_from_string(
        "<p>Address: " + res.details.address + "</p>"));
  if (res.details.city)
    parent.appendChild(
      element_from_string(
        "<p>City: " + res.details.city + "</p>"));
}

function click_check_swift()
{
  var swift = document.getElementById("swift").value;
  check_swift(
    swift,
    function(x)
      {
        clear_swift_results();
        display_swift_results(x);
      });
}
