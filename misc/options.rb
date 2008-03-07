def allowed_options {
 :just_syntax          => {:type => 'bool', :default => false,  :description => 'Just check the syntax of input files'},
 :no_warnings          => {:type => 'bool', :default => false,  :description => 'Suppress warnings'},
 :no_check             => {:type => 'bool', :default => false,  :description => 'Disable basic semantic checks'},
 :assigning_to_args    => {:type => 'wei',  :default => 'i',    :description => 'Assigning to arguments'},
 :bad_regexps          => {:type => 'wei',  :default => 'e',    :description => 'Bad regular expression litterals'}
 :dangling_commas      => {:type => 'wei',  :default => 'e',    :description => 'Dangling commas in object and array litterals'},
 :shadowing_args       => {:type => 'wei',  :default => 'w',    :description => 'Arguments shadowed by variables'},
 :toplevel_bindings    => {:type => 'wei',  :default => 'i',    :description => 'Setting global properties (without a "var")'},
 :unbound_variables    => {:type => 'wei',  :default => 'w',    :description => 'Assigning to apparently unbound global variables, except in the toplevel'},
 :undefined_variables  => {:type => 'wei',  :default => 'i',    :description => 'Referring to global names not obviously declared'},
 :uninitialized_vars   => {:type => 'wei',  :default => 'w',    :description => 'Uninitialized variables'},
 :unreachable_code     => {:type => 'wei',  :default => 'w',    :description => 'Unreachable code'},
 :unused_args          => {:type => 'wei',  :default => 'w',    :description => 'Unused arguments'},
 :unused_funs          => {:type => 'wei',  :default => 'w',    :description => 'Unused functions'},
 :unused_vars          => {:type => 'wei',  :default => 'w',    :description => 'Unused variables'},
 :using_unused         => {:type => 'wei',  :default => 'w',    :description => 'Using variables or arguments lexically declared to be unused'},

 # typecheck is too beta
 #:typecheck            => {:type => 'bool', :default => true,   :description => 'Attempt to typecheck source files'},
}
