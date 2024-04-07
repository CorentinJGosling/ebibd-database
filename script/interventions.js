var modal = document.getElementById("myModal");

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// Function to handle click on button
function handleClick(event) {
  var content = event.target.dataset.content;
  document.getElementById("modal-content").textContent = content;
  modal.style.display = "block";
}

// When the user clicks on a cell in the fourth column, replace its content with a button
var fourthColumnCells = document.querySelectorAll("tr > td:last-child");
fourthColumnCells.forEach(function (cell) {
  cell.innerHTML =
    '<button class="modal-button" data-content="' +
    cell.textContent +
    '">Click</button>';
});

// Add event listeners to the buttons
var buttons = document.querySelectorAll(".modal-button");
buttons.forEach(function (button) {
  button.addEventListener("click", handleClick);
});

// When the user clicks on <span> (x), close the modal
span.onclick = function () {
  modal.style.display = "none";
};

// When the user clicks anywhere outside of the modal, close it
window.onclick = function (event) {
  if (event.target == modal) {
    modal.style.display = "none";
  }
};

const navbar = document.querySelector("nav");
window.addEventListener("scroll", () => {
  if (window.scrollY > 0) {
    navbar.classList.add("scrolled");
  } else {
    navbar.classList.remove("scrolled");
  }
});
const navLinks = document.querySelector(".nav-links");
var navIcons = document.querySelectorAll("#hamburger-menu");

navIcons.forEach(function (navIcon) {
  navIcon.addEventListener("click", function () {
    navIcon.classList.toggle("open");
    navLinks.classList.toggle("open");
  });
});
const navLinksli = document.querySelectorAll(".nav-links li");

navLinksli.forEach((link) => {
  link.addEventListener("click", () => {
    const href = link.querySelector("a").getAttribute("href");
    window.location.href = href;
  });
});

// $(document).ready(function () {
//   $.ajax({
//     url: "../interventions/interventions_list.html",
//     dataType: "html",
//     success: function (data) {
//       $("#data-table").html(data);
//     },
//   });
// });

const content = document.querySelectorAll(".cont");

function filterTable() {
  var nameFilter = $("#filter1").val() || [];
  $("#data-table tbody tr").each(function () {
    var name = $(this).find("td:eq(0)").text();
    if (nameFilter.length === 0 || nameFilter.indexOf(name) !== -1) {
      $(this).show();
    } else {
      $(this).hide();
    }
  });
}
$("#filter1").on("change", function () {
  filterTable();
});

$(document).ready(function () {
  $(".js-example-basic-multiple").select2({});
  $("#data-table").load(
    "../interventions/interventions_list.html",
    function () {
      // Populate filtering panel with unique values from the table
      var nameOptions = [];
      $("#data-table tbody tr").each(function () {
        var name = $(this).find("td:eq(0)").text();
        if ($.inArray(name, nameOptions) == -1) nameOptions.push(name);
      });
      nameOptions.sort();
      $.each(nameOptions, function (i, value) {
        $("#filter1").append(
          '<option value="' + value + '">' + value + "</option>"
        );
      });
    }
  );
});

// Themes begin
am4core.useTheme(am4themes_animated);
// Themes end

var chart = am4core.create("chartdiv", am4plugins_wordCloud.WordCloud);
var series = chart.series.push(new am4plugins_wordCloud.WordCloudSeries());
chart.logo.disabled = true;

series.accuracy = 4;
series.step = 50;
series.rotationThreshold = 0.7;
series.maxCount = 100;
series.minWordLength = 2;
series.labels.template.tooltipText = "{word} (n comparisons = {value})";
series.fontFamily = "Courier New";
series.maxFontSize = am4core.percent(30);

series.text =
  "acetyl-L-carnitine+alpha-lipoic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , acetylsalicylic acid , agomelatin , agomelatin , agomelatin , agomelatin , agomelatin , agomelatin , allopurinol, allopurinol , allopurinol , allopurinol , allopurinol , allopurinol , allopurinol , allopurinol , allopurinol , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , amitriptyline , aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole, aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole , aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole LAI, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+divalproate, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lamotrigine, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, aripiprazole+lithium/divalproate, armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , armodafinil , asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, asenapine, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, brexpiprazole, bright light therapy , bright light therapy , bright light therapy , bright light therapy , bright light therapy , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , bupropion , carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine, carbamazepine+FEWP, carbamazepine+FEWP, carer focused intervention , carer focused intervention , carer focused intervention , cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, cariprazine, CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT , CBT group , CBT group , CBT group , CBT+psychoeducation , CBT+psychoeducation , CBT+psychoeducation , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , celecoxib , chlorpromazine, chlorpromazine, chlorpromazine, chlorpromazine, choline , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , citalopram , clonazepam, clonazepam, clonazepam, clonazepam, coenzyme Q10 , coenzyme Q10 , coenzyme Q10 , coenzyme Q10 , coenzyme Q10 , coenzyme Q10 , creatine , creatine , creatine , creatine , creatine , creatine , dexamphetamine , dextromethorphan , dextromethorphan , dextromethorphan , dextromethorphan , dextromethorphan , dextromethorphan , divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, divalproate, ECT , ECT , ECT , ECT , ECT , ECT , erythropoietin , erythropoietin , erythropoietin , esamisulpride, esamisulpride, escitalopram, escitalopram, escitalopram, escitalopram, escitalopram, escitalopram, esketamine , eslicarbazepine, eslicarbazepine, eslicarbazepine, eslicarbazepine, eslicarbazepine, family focused therapy , family focused therapy , family focused therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , family/conjoint therapy , fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine, fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , fluoxetine , folic acid , functional remediation , functional remediation , gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, gabapentin, galantamine , haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol, haloperidol+lithium/divalproate, haloperidol+lithium/divalproate, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine, imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , imipramine , infliximab , infliximab , infliximab , infliximab , infliximab , infliximab , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , inositol , insulin , IPSRT , IPSRT , IPSRT , IPSRT , IPSRT , IPSRT , IPSRT , ketamine , ketamine , ketamine , ketamine , ketamine , ketamine , ketamine , ketamine , ketamine , lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine, lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , lamotrigine , levetiracetam , levetiracetam , levetiracetam , levetiracetam , levetiracetam , levetiracetam , licarbazepine, licarbazepine, licarbazepine, licarbazepine, lisdexamfetamine , lisdexamfetamine , lisdexamfetamine , lisdexamfetamine , lisdexamfetamine , lisdexamfetamine , lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium, lithium , lithium , lithium , lithium , lithium , lithium , lithium+imipramine, lithium+imipramine, lithium+imipramine, lithium+imipramine, lumateperone, lumateperone, lumateperone , lumateperone , lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone, lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone , lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, lurasidone+lithium/divalproate, melatonin, melatonin , memantine , memantine , memantine , memantine , memantine , memantine , memantine , memantine , memantine , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline , minocycline+acetylsalicylic acid , minocycline+acetylsalicylic acid , minocycline+acetylsalicylic acid , minocycline+acetylsalicylic acid , minocycline+acetylsalicylic acid , minocycline+acetylsalicylic acid , minocycline+celecoxib , modafinil , modafinil , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine , n-acetyl cysteine+acetylsalicylic acid , n-acetyl cysteine+acetylsalicylic acid , n-acetyl cysteine+acetylsalicylic acid , n-acetyl cysteine+acetylsalicylic acid , n-acetyl cysteine+acetylsalicylic acid , n-acetyl cysteine+acetylsalicylic acid , naltrexone , olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine, olanzapine , olanzapine , olanzapine , olanzapine , olanzapine , olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine, olanzapine+fluoxetine , olanzapine+fluoxetine , olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, olanzapine+lithium/divalproate, omega3 , omega3 , omega3 , omega3 , omega3 , omega3 , omega3 , omega3 , omega3 , omega3 , paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paliperidone, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine, paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine , paroxetine+bupropion , paroxetine+bupropion , paroxetine+bupropion , paroxetine+bupropion , paroxetine+bupropion , phenelzine, phenelzine, phenelzine, phenelzine, phenelzine, phenelzine, pindolol , pindolol , pindolol , pindolol , pindolol , pindolol , pioglitazone , pioglitazone , pioglitazone , pioglitazone , pioglitazone , pioglitazone , pramipexole , pramipexole , pramipexole , pramipexole , pramipexole , pramipexole , pregnenolone , pregnenolone , pregnenolone , pregnenolone , pregnenolone , pregnenolone , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation , psychoeducation brief , psychoeducation brief , psychoeducation group , psychoeducation group , psychoeducation group , psychoeducation group , psychoeducation group , psychoeducation individual , psychoeducation individual , psychoeducation individual , quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine, quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine , quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, quetiapine+lithium/divalproate, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon, ramelteon , ramelteon , riluzole, riluzole, riluzole , risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone, risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone , risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, risperidone LAI, rTMS , rTMS , rTMS , rTMS , rTMS , rTMS , rTMS , sAME , sAME , sAME , sAME , sAME , sAME , selegiline, sertraline, sertraline, sertraline , sertraline , sertraline , sertraline , sertraline , sertraline , t3 hormone , t3 hormone , t3 hormone , t3 hormone , t3 hormone , t3 hormone , t4 hormone , t4 hormone , t4 hormone , t4 hormone , t4 hormone , t4 hormone , tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen, tamoxifen , tamoxifen , topiramate, topiramate, topiramate, topiramate, topiramate, topiramate, topiramate, total sleep deprivation , valnoctamide, valnoctamide, valnoctamide, valnoctamide, valnoctamide, valnoctamide, verapamil, verapamil, verapamil, verapamil, verapamil, verapamil, vitD3 , vitD3 , vitD3 , vitD3 , vitD3 , vitD3 , withania somnifer , withania somnifer , ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone, ziprasidone , ziprasidone , ziprasidone , ziprasidone , ziprasidone , ziprasidone , ziprasidone , ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, ziprasidone+lithium/divalproate, zonisamide , zonisamide";
