[{class,
    [{name, "MathUtils"},
     {constraints, [{invariants, []}]},
     {attributes, []},
     {operations,
        [{operation,
            [{name, "add"},
             {params,
                [{param, [{name, "a"}, {type, [{name, "int"}]}]},
                 {param, [{name, "b"}, {type, [{name, "int"}]}]}]},
             {constraints,
                [{preconditions, []},
                 {postconditions,
                    [{constraint,
                        [{name, ""},
                         {body,
                            [{expression, "OperationCallExp"},
                             {name, ""},
                             {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                             {contents,
                                [{content,
                                    [{expression, "VariableExp"},
                                     {referredVariable,
                                        [{expression, "Variable"},
                                         {name, "result"},
                                         {type, [{name, "int"}]}]}]},
                                 {content,
                                    [{expression, "OperationCallExp"},
                                     {name, ""},
                                     {referredOperation,
                                        [{name, "+"}, {type, [{name, "Integer"}]}]},
                                     {contents,
                                        [{content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "a"},
                                                 {type, [{name, "int"}]}]}]},
                                         {content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "b"},
                                                 {type, [{name, "int"}]}]}]}]}]}]}]}]}]},
                 {invariants, []}]}]},
         {operation,
            [{name, "mult"},
             {params,
                [{param, [{name, "a"}, {type, [{name, "int"}]}]},
                 {param, [{name, "b"}, {type, [{name, "int"}]}]}]},
             {constraints,
                [{preconditions, []},
                 {postconditions,
                    [{constraint,
                        [{name, ""},
                         {body,
                            [{expression, "OperationCallExp"},
                             {name, ""},
                             {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                             {contents,
                                [{content,
                                    [{expression, "VariableExp"},
                                     {referredVariable,
                                        [{expression, "Variable"},
                                         {name, "result"},
                                         {type, [{name, "int"}]}]}]},
                                 {content,
                                    [{expression, "OperationCallExp"},
                                     {name, ""},
                                     {referredOperation,
                                        [{name, "*"}, {type, [{name, "Integer"}]}]},
                                     {contents,
                                        [{content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "a"},
                                                 {type, [{name, "int"}]}]}]},
                                         {content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "b"},
                                                 {type, [{name, "int"}]}]}]}]}]}]}]}]}]},
                 {invariants, []}]}]},
         {operation,
            [{name, "pow"},
             {params,
                [{param, [{name, "a"}, {type, [{name, "int"}]}]},
                 {param, [{name, "b"}, {type, [{name, "positiveInteger"}]}]}]},
             {constraints,
                [{preconditions, []},
                 {postconditions,
                    [{constraint,
                        [{name, ""},
                         {body,
                            [{expression, "OperationCallExp"},
                             {name, ""},
                             {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                             {contents,
                                [{content,
                                    [{expression, "VariableExp"},
                                     {referredVariable,
                                        [{expression, "Variable"},
                                         {name, "result"},
                                         {type, [{name, "int"}]}]}]},
                                 {content,
                                    [{expression, "IteratorExpImpl"},
                                     {name, "iterate"},
                                     {iterator,
                                        [{variable,
                                            [{expression, "Variable"},
                                             {name, "i"},
                                             {type, [{name, "Integer"}]}]}]},
                                     {result,
                                        [{expression, "Variable"},
                                         {name, "i"},
                                         {type,
                                            [{name, "Integer"},
                                             {initExpression,
                                                [{expression, "IntegerLiteralExp"},
                                                 {value, 1}]}]}]},
                                     {source,
                                        [{expression, "CollectionLiteralExp"},
                                         {kind, "Sequence"},
                                         {range,
                                            [{first,
                                                [{expression, "IntegerLiteralExp"}, {value, 1}]},
                                             {last,
                                                [{expression, "VariableExp"},
                                                 {referredVariable,
                                                    [{expression, "Variable"},
                                                     {name, "b"},
                                                     {type, [{name, "positiveInteger"}]}]}]}]}]},
                                     {body,
                                        [{expression, "OperationCallExp"},
                                         {name, ""},
                                         {referredOperation,
                                            [{name, "*"}, {type, [{name, "Integer"}]}]},
                                         {contents,
                                            [{content,
                                                [{expression, "VariableExp"},
                                                 {referredVariable,
                                                    [{expression, "Variable"},
                                                     {name, "acc"},
                                                     {type, [{name, "Integer"}]}]}]},
                                             {content,
                                                [{expression, "VariableExp"},
                                                 {referredVariable,
                                                    [{expression, "Variable"},
                                                     {name, "a"},
                                                     {type, [{name, "int"}]}]}]}]}]}]}]}]}]}]},
                 {invariants, []}]}]},
         {operation,
            [{name, "division"},
             {params,
                [{param, [{name, "a"}, {type, [{name, "int"}]}]},
                 {param, [{name, "b"}, {type, [{name, "int"}]}]}]},
             {constraints,
                [{preconditions,
                    [{constraint,
                        [{name, ""},
                         {body,
                            [{expression, "OperationCallExp"},
                             {name, ""},
                             {referredOperation, [{name, "<>"}, {type, [{name, "Boolean"}]}]},
                             {contents,
                                [{content,
                                    [{expression, "VariableExp"},
                                     {referredVariable,
                                        [{expression, "Variable"},
                                         {name, "b"},
                                         {type, [{name, "int"}]}]}]},
                                 {content, [{expression, "IntegerLiteralExp"}, {value, 0}]}]}]}]}]},
                 {postconditions,
                    [{constraint,
                        [{name, ""},
                         {body,
                            [{expression, "OperationCallExp"},
                             {name, ""},
                             {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                             {contents,
                                [{content,
                                    [{expression, "VariableExp"},
                                     {referredVariable,
                                        [{expression, "Variable"},
                                         {name, "result"},
                                         {type, [{name, "int"}]}]}]},
                                 {content,
                                    [{expression, "OperationCallExp"},
                                     {name, ""},
                                     {referredOperation,
                                        [{name, "/"}, {type, [{name, "Integer"}]}]},
                                     {contents,
                                        [{content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "a"},
                                                 {type, [{name, "int"}]}]}]},
                                         {content,
                                            [{expression, "VariableExp"},
                                             {referredVariable,
                                                [{expression, "Variable"},
                                                 {name, "b"},
                                                 {type, [{name, "int"}]}]}]}]}]}]}]}]},
                     {constraint,
                        [{name, "testif"},
                         {body,
                            [{expression, "IfExp"},
                             {condition,
                                [{expression, "OperationCallExp"},
                                 {name, ""},
                                 {referredOperation, [{name, "<>"}, {type, [{name, "Boolean"}]}]},
                                 {contents,
                                    [{content,
                                        [{expression, "VariableExp"},
                                         {referredVariable,
                                            [{expression, "Variable"},
                                             {name, "b"},
                                             {type, [{name, "int"}]}]}]},
                                     {content, [{expression, "IntegerLiteralExp"}, {value, 0}]}]}]},
                             {then,
                                [{expression, "OperationCallExp"},
                                 {name, ""},
                                 {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                                 {contents,
                                    [{content,
                                        [{expression, "VariableExp"},
                                         {referredVariable,
                                            [{expression, "Variable"},
                                             {name, "result"},
                                             {type, [{name, "int"}]}]}]},
                                     {content,
                                        [{expression, "OperationCallExp"},
                                         {name, ""},
                                         {referredOperation,
                                            [{name, "/"}, {type, [{name, "Integer"}]}]},
                                         {contents,
                                            [{content,
                                                [{expression, "VariableExp"},
                                                 {referredVariable,
                                                    [{expression, "Variable"},
                                                     {name, "a"},
                                                     {type, [{name, "int"}]}]}]},
                                             {content,
                                                [{expression, "VariableExp"},
                                                 {referredVariable,
                                                    [{expression, "Variable"},
                                                     {name, "b"},
                                                     {type, [{name, "int"}]}]}]}]}]}]}]},
                             {else,
                                [{expression, "OperationCallExp"},
                                 {name, ""},
                                 {referredOperation, [{name, "="}, {type, [{name, "Boolean"}]}]},
                                 {contents,
                                    [{content,
                                        [{expression, "VariableExp"},
                                         {referredVariable,
                                            [{expression, "Variable"},
                                             {name, "result"},
                                             {type, [{name, "int"}]}]}]},
                                     {content,
                                        [{expression, "IntegerLiteralExp"}, {value, 0}]}]}]}]}]}]},
                 {invariants, []}]}]}]}]},
 {class,
    [{name, "pow"},
     {constraints, [{invariants, []}]},
     {attributes,
        [{attribute, [{name, "a"}, {type, [{name, "int"}]}]},
         {attribute, [{name, "b"}, {type, [{name, "positiveInteger"}]}]}]},
     {operations, []}]},
 {class,
    [{name, "mult"},
     {constraints, [{invariants, []}]},
     {attributes,
        [{attribute, [{name, "a"}, {type, [{name, "int"}]}]},
         {attribute, [{name, "b"}, {type, [{name, "int"}]}]}]},
     {operations, []}]},
 {class,
    [{name, "division"},
     {constraints, [{invariants, []}]},
     {attributes,
        [{attribute, [{name, "a"}, {type, [{name, "int"}]}]},
         {attribute, [{name, "b"}, {type, [{name, "int"}]}]}]},
     {operations, []}]},
 {class,
    [{name, "add"},
     {constraints, [{invariants, []}]},
     {attributes,
        [{attribute, [{name, "a"}, {type, [{name, "int"}]}]},
         {attribute, [{name, "b"}, {type, [{name, "int"}]}]}]},
     {operations, []}]}].