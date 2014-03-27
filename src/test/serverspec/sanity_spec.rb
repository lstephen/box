require 'spec_helper'

describe "Sanity" do
    specify { true.should be_true }
    specify { false.should be_false }
end

describe command('whoami') do
    it { should return_stdout 'root' }
end

describe command('whoami') do
    let(:disable_sudo) { true }
    it { should return_stdout 'vagrant' }
end

