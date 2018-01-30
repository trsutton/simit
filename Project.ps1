$project = "simit.ipkg"
$testpkg = "test.ipkg"

function SimIt-Clean {
    echo "clean project"
    idris --clean $project
    
    cd test; 
    idris --clean $testpkg; 
    
    cd ..
}

function SimIt-Test {
    echo "test project"
    
    cd test
    idris --testpkg $testpkg
    
    If ( $? ) { echo "SUCCESSFUL" }
    Else { echo "FAILURE"; cd ..; exit 1; }

    cd ..
}

function SimIt-BuildNoTest() {
    echo "build project"
    idris --build $project
}

function SimIt-Build() {
    SimIt-BuildNoTest
    SimIt-Test
}

function SimIt-Install() {
    echo "install project"
    SimIt-Clean
    SimIt-Test

    If ( $? ) { idris --install $project }
    Else { echo "couldn't install project > something went wrong"; exit 1 }
}

function SimIt-Repl() {
    echo "Starting Idris repl..."
    idris -p contrib -p effects -i src
}

function SimIt-TestRepl() {
    echo "Starting Idris repl got test..."
    idris -p contrib -p effects -p specdris -i src -i test
}

function SimIt-Help() {
    echo "usage: 'Project.ps1 [command]'"
    echo "  --clean: cleans the project"
    echo "  --build: builds the code and runs all tests"
    echo "  --buildNoTest: builds the code only"
    echo "  --repl: Starts a new repl including packages used by the application"
    echo "  --test: runs all tests"
    echo "  --test-repl: Starts a new repl including packages used by the application/test" 
    echo "  --install: installs the project"
    echo "  --help: prints the help"
}

switch ($args[0]) {
    "--clean" { SimIt-Clean }
    "--build" { SimIt-Build }
    "--buildNoTest" { SimIt-BuildNoTest }
    "--repl" { SimIt-Repl }
    "--test" { SimIt-Test }
    "--test-repl" { SimIt-TestRepl }
    "--install" { SimIt-Install }
    "--help" { SimIt-Help }
    default { echo "unknown command"; SimIt-Help }
}