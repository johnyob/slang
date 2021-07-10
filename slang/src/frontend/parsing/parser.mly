%{

module Located = Syntax.Located
open Syntax.Located
open Location

open Syntax.Frontend.Parsed

open Literal
open Pattern
open Binder
open Binding
open Expr

(** TODO: Improve location tracking using indicies *)
let loc () =
  let pos1 = Parsing.symbol_start_pos () 
  and pos2 = Parsing.symbol_end_pos () in
    Location.({ file_path=pos1.pos_fname
    ; span=(Position (pos1.pos_lnum, pos1.pos_cnum - pos1.pos_bol + 1), Position (pos2.pos_lnum, pos2.pos_cnum - pos2.pos_bol + 1)) 
    })
    
%}

/* Tokens and types */

%token<string> LID
%token<string> UID

%token EOF LPAREN RPAREN SEMICOLON LBRACE RBRACE // BAR COLON COMMA

%token PLUS MINUS STAR SLASH ASSIGN BANG EQUALS LT LEQ GT GEQ AND OR

%token IF THEN ELSE LET IN LAMBDA ARROW CASE OF WHILE DO FIX REC 

%token<int> LINT
%token<bool> LBOOL
%token LUNIT

/* Comparison operators */
%right EQUALS LT LEQ GT GEQ

/* Arithmetic operators */
%left PLUS MINUS
%left STAR SLASH

/* Assignment, right associative for chaining */
%right ASSIGN              
%nonassoc BANG            

%nonassoc THEN ELSE ARROW IN OF DO REC 

// %nonassoc UNARY_MINUS

%start <Module.t> program

%type <Module.t> _module
%type <Binder.located> binder
%type <Binding.located> binding
%type <Literal.located> literal
%type <Pattern.located> pattern
%type <Expr.located_branch> branch
%type <Expr.located_branch list> branches
%type <Expr.located> expr
%type <Expr.located> atom
%type <Lid.located> lid
%type <Uid.located> uid


%%

program:
  | _module; EOF                                                { $1 }

_module:
  | nonempty_list(terminated(binder, SEMICOLON))                { $1 }

binder:
  | LET; REC; LBRACE; bs = nonempty_list(terminated(binding, SEMICOLON)); RBRACE                       
    { (Located.fold bs, Rec bs)  }
  | LET; b = binding                                            
    { (location b, NonRec b)      }

binding:
  | x = lid; ps = nonempty_list(pattern); EQUALS; e = expr      { (x <@> e, BFunction (x, ps, e)) }
  | x = lid; EQUALS; e = expr                                   { (x <@> e, BValue (x, e))        }

literal:
  | n = LINT                                                    { (loc (), LInt n)                }
  | b = LBOOL                                                   { (loc (), LBool b)               }
  | LUNIT                                                       { (loc (), LUnit)                 }

pattern:
  | x = uid; ps = list(pattern)                               { (location x <+> Located.fold ps, PCon (x, ps)) }
  | x = lid                                                   { (loc (), PVar x)                    }
  | l = literal                                               { (location l, PLit (l))                }
  | LPAREN; p = pattern; RPAREN                               { p                                     }
  
branches:
  | bs = list(terminated(branch, SEMICOLON))                    { bs }

branch:
  | p = pattern; ARROW; e = expr                                { (p <@> e, (p, e))                     }

expr:
  | FIX; f = lid; ps = nonempty_list(pattern); ARROW; e = expr  { (Located.fold ps <+> location e, EFix (f, ps, e))           }
  | LAMBDA; ps = nonempty_list(pattern); ARROW; e = expr        { (Located.fold ps <+> location e, ELam (ps, e))              }
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr              { (c <@> e2, EIf (c, e1, e2))                         }
  | b = binder; IN; e=expr                                      { (b <@> e, ELet (b, e))                             }
  | CASE; e=expr; OF; LBRACE; bs=branches; RBRACE             { (location e <+> Located.fold bs, ECase (e, bs))             }
  | WHILE; e1 = expr; DO; e2 = expr                             { (e1 <@> e2, EWhile (e1, e2))                        }
  | LBRACE; es = list(terminated(expr, SEMICOLON)); RBRACE      { (Located.fold es, ESeq (es))                       }
  | op=uop; e1 = expr;                                          { (op <@> e1, EApp (op, e1))                          }
  | e1 = expr; op=bop; e2 = expr                                { (e1 <@> e2, EApp ((e1 <@> op, EApp (op, e1)), e2))  }
  | e1 = expr; e2 = atom                                        { (e1 <@> e2, EApp (e1, e2))                          }
  | atom                                                        { $1 }

atom:
  | x = lid                                       { (loc (), EVar x)              }
  | x = uid                                       { (loc (), ECon x)              }
  | l=literal                                     { (location l, ELit l)          }
  | LPAREN; e = expr; RPAREN                      { e                             }

lid:
  | x = LID                                       { (loc (), x) }

uid:
  | x = UID                                       { (loc (), x) }

%inline uop:
  | MINUS                                         { (loc (), EVar (loc (), "(neg)"))    }
  | BANG                                          { (loc (), EVar (loc (), "(!)"))      }

%inline bop:
  | PLUS                            { (loc (), EVar (loc (), "(+)"))    }
  | MINUS                           { (loc (), EVar (loc (), "(-)"))    }
  | STAR                            { (loc (), EVar (loc (), "(*)"))    }
  | SLASH                           { (loc (), EVar (loc (), "(/)"))    }
  | EQUALS                          { (loc (), EVar (loc (), "(=)"))    }
  | LT                              { (loc (), EVar (loc (), "(<)"))    }
  | LEQ                             { (loc (), EVar (loc (), "(<=)"))   }
  | GT                              { (loc (), EVar (loc (), "(>)"))    }
  | GEQ                             { (loc (), EVar (loc (), "(>=)"))   }
  | AND                             { (loc (), EVar (loc (), "(&&)"))   }
  | OR                              { (loc (), EVar (loc (), "(||)"))   }
  | ASSIGN                          { (loc (), EVar (loc (), "(:=)"))   }
  
