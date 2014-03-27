require 'spec_helper'

describe package('ansible') do
    it { should be_installed.by('pip').with_version('1.5.3') }
end

