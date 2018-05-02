class Superclass
  FIND_ME = "Found in superclass"
end

module ParentLexicalScope
  FIND_ME = "Found in parentLexical scope"

  module ChildLexicalScope
    class Subclass < Superclass
      p FIND_ME
    end
  end

end
