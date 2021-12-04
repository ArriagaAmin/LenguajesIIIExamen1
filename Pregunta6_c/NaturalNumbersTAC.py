import re 
from sys import argv
from typing import *

# Tokens reservados.
reserved = {
  '\n'       : 'NL',
  ':='       : 'ASSIGN',
  ':'        : 'COLON',
  '+'        : 'ADD',
  '-'        : 'SUB',
  '*'        : 'MUL',
  '/'        : 'DIV',
  '<'        : 'LT', 
  '>'        : 'GT',
  '=='       : 'EQ',
  '!='       : 'NEQ',
  'advance'  : 'ADVANCE',
  'goto'     : 'GOTO',
  'goif'     : 'GOIF'
}
# Tokens con regex
regex = {
  '[0-9]+' : 'NUM',
  '[A-Za-z_][A-Za-z0-9_]*' : 'ID'
}


class Parser:
  """
    Implementacion del parser recursivo descendente para el TAC: 
    
    TAC   =>  LINE \\n TAC
          |  LINE $
    LINE  =>  id : INSTR
          |  INSTR
    INSTR =>  id := ARIT
          |  advance id
          |  goto id
          |  goif CMP id
    ARIT  =>  VAL + VAL
          |  VAL − VAL
          |  VAL ∗ VAL
          |  VAL / VAL
    CMP   =>  VAL < VAL
          |  VAL > VAL
          |  VAL == VAL
          |  VAL != VAL
    VAL   =>  id
          |  num
    
    Ademas, verifica cuando una division es riesgosa debido a que el denominador puede
    ser cero.
  """

  def __init__(self):
    # Lista con los tokens de la entrada
    self.entry = []

  def lexer(self, text: str) -> bool:
    """ 
      Realiza las verificaciones lexicas del texto, separandolo en tokens validos. 

      Parametros:
      -----------
        * text (str): Texto a analizar.

      Returns:
      --------
        * bool: Indica si no hay errores lexicos.
    """
    # Eliminamos tabuladores
    text = text.replace('\t', '')
    # Agregamos espacio entre cada token reservado
    for r in reserved:
      text = text.replace(r, f' {r} ')
    text = text.replace(': =', ':=')

    # Obtenemos los tokens
    tokens = text.split(' ')

    # Verificamos que no hayan errores lexicos.
    line = 1
    errors = []
    for t in tokens:
      # Los tokens reservados son validos
      if t in reserved:
        self.entry.append((reserved[t], t, line))
        if t == '\n':
          line += 1
        continue
       # En cambio si es '', ignorar
      elif t == '':
      	continue

      # Verificamos si corresponde a alguno de los tokens con regex
      match = False
      for r in regex:
        pattern = re.compile(r)
        if pattern.fullmatch(t):
          self.entry.append((regex[r], t, line))
          match = True
          break

      # Si no corresponde, token invalido.
      if not match:
        errors.append(f'{line}: Invalid token \033[1;3m{t}\033[0m.')
        continue 

    # Verificamos si hay errores
    if not errors:
      self.entry += [('END', '$')]
      return True
    else:
      self.entry = []
      for e in errors: print(f'\033[1;31mLexical Error\033[0m:{e}')
      return False

  def shift(self, symbol: str) -> bool:
    """ 
      Verificamos que el siguiente simbolo de la entrada sea el mismo que al que se le 
      esta haciendo shift.

      Parametros:
      -----------
        * symbol (str): Token al que se le hace shift.

      Returns:
      --------
        * bool: El token corresponde con la entrada.
    """
    if self.entry[0][0] == symbol:
      self.entry.pop(0)
      return True 
    return False

  def LA(self, i: int) -> Tuple[str, str]:
    """ Lookahead. """
    if len(self.entry) > i: return self.entry[i]
    return ''

  def parse(self, filename: str) -> Tuple[bool, dict, set, set]:
    """
      Realiza las verificaciones lexicas y sintacticas del texto en un archivo. 

      Parametros:
      -----------
        * filename (str): Path al archivo con el texto.

      Returns:
      --------
        * bool: Indica si no hay errores lexicos o sintacticos.
        * dict: Diccionario de etiquetas a lineas donde se definieron
        * set: Conjunto de lineas lideres.
        * set: Conjunto de variables usadas.
    """
    with open(filename, 'r') as file:
      text = file.read()

      if not self.lexer(text):
         return (False, {}, set(), set())

      # Etiquetas definidas en el codigo
      labels_def = {}
      # Variables usadas en el codigo
      used_vars = set()
      # Conjunto de instrucciones lideres
      leaders = set()
      # Linea actual
      line = 1

      if not self.TAC(labels_def, leaders, used_vars, line):
        return (False, {}, set(), set())

      # Verificamos que todas las etiquetas usadas fueron definidas
      real_leaders = set()
      for label in leaders:
        # Si el lider es una etiqueta, agregamos la linea donde fue definida
        if isinstance(label, str):
          if label in labels_def:
            real_leaders.add(labels_def[label])

          # Si no fue definida en ninguna linea, error
          else:
            print(f'\033[1;31mSyntax Error\033[0m: Undefined label \033[1;3m{label}\033[m.')
            self.errors = True
        else:
          real_leaders.add(label)

    return (True, labels_def, real_leaders, used_vars)


  # ========================== PRODUCCIONES ========================== #
  # Todas las producciones retornan un booleano que indica si se pudo ejecutar dicha
  # produccion o, en caso contrario, hubo un error de parseo.

  def TAC(self, labels_def: dict, leaders: set, used_vars: set, line: int) -> bool:
    # Procesamos la produccion LINE
    if not self.LINE(labels_def, leaders, used_vars, line):
      return False 

    la = self.LA(0)

    if la[0] == 'NL':
      # Estamos en la produccion  TAC => LINE \n TAC
      self.shift(la[0])
      return self.TAC(labels_def, leaders, used_vars, line+1)

    elif la[0] == 'END': 
      # Estamos en la produccion  TAC => LINE $
      self.shift(la[0])
      return True 

    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

  def LINE(self, labels_def: set, leaders: set, used_vars: set, line: int) -> bool:
    la0 = self.LA(0)
    la1 = self.LA(1)

    if la0[0] == 'ID' and la1[0] == 'COLON':
      # Estamos en la produccion  LINE => id : INSTR
      self.shift(la0[0])
      self.shift(la1[0])

      if la0[1] in labels_def:
        print(
          f'\033[1;31mSyntax Error\033[0m:{line}: Label \033[1;3m{la0[1]}\033[m was defined ' +\
          f'in line \033[1;3m{labels_def[la0[1]]}\033[m.'
        )
      labels_def[la0[1]] = line
    
    return self.INSTR(leaders, used_vars, line)

  def INSTR(self, leaders: set, used_vars: set, line: int) -> bool:
    la = self.LA(0)

    if la[0] == 'ID':
      # Estamos en la produccion  INSTR => id := ARIT
      self.shift(la[0])

      used_vars.add(la[1])

      if not self.shift('ASSIGN'):
        # Error 
        la = self.LA(0)
        print(f'\033[1;31mSyntax Error\033[0m:{line}: Unexpected token \033[1;3m{la[1]}\033[m.')
        return False

      return self.ARIT(used_vars)

    elif la[0] == 'ADVANCE':
      # Estamos en la produccion  INSTR => advance id
      self.shift(la[0])

      la = self.LA(0)
      if not self.shift('ID'):
        # Error 
        print(f'\033[1;31mSyntax Error\033[0m:{line}: Unexpected token \033[1;3m{la[1]}\033[m.')
        return False

      used_vars.add(la[1])

      return True 

    elif la[0] == 'GOTO':
      # Estamos en la produccion  INSTR => goto id
      self.shift(la[0])

      la = self.LA(0)
      if not self.shift('ID'):
        # Error 
        print(f'\033[1;31mSyntax Error\033[0m:{line}: Unexpected token \033[1;3m{la[1]}\033[m.')
        return False

      leaders.add(line+1)
      leaders.add(la[1])

      return True 

    elif la[0] == 'GOIF':
      # Estamos en la produccion  INSTR => goif CMP id
      self.shift(la[0])

      if not self.CMP():
        return False

      la = self.LA(0)
      if not self.shift('ID'):
        # Error 
        print(f'\033[1;31mSyntax Error\033[0m:{line}: Unexpected token \033[1;3m{la[1]}\033[m.')
        return False

      leaders.add(line+1)
      leaders.add(la[1])

      return True 

    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{line}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

  def ARIT(self, used_vars: set) -> bool:
    la = self.LA(0)
    if la[0] in {'ID', 'NUM'}:
      self.shift(la[0])
      if la[0] == 'ID':
        used_vars.add(la[1])
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    la = self.LA(0)
    if la[0] in {'ADD', 'SUB', 'MUL', 'DIV'}:
      self.shift(la[0])

      # Verificamos si se esta haciendo una division entre el literal '0'
      if la[0] == 'DIV':
        la = self.LA(0)
        if la[1] == '0':
          print(f'\033[1;33mWarning\033[0m:{la[2]}: Potencial division by zero.')
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    la = self.LA(0)
    if la[0] in {'ID', 'NUM'}:
      self.shift(la[0])
      if la[0] == 'ID':
        used_vars.add(la[1])
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    return True

  def CMP(self) -> bool:
    la = self.LA(0)
    if la[0] in {'ID', 'NUM'}:
      self.shift(la[0])
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    la = self.LA(0)
    if la[0] in {'LT', 'GT', 'EQ', 'NEQ'}:
      self.shift(la[0])
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    la = self.LA(0)
    if la[0] in {'ID', 'NUM'}:
      self.shift(la[0])
    else:
      # Error 
      print(f'\033[1;31mSyntax Error\033[0m:{la[2]}: Unexpected token \033[1;3m{la[1]}\033[m.')
      return False

    return True

class FlowGraphNode:
  """
    Nodo de un grafo de flujo, es decir, bloque de codigo contiguo.
  """
  def __init__(self, lines: List[str], id: int, leader: int, OUT: set):
    self.id = id
    self.leader = leader
    self.IN = set()
    self.OUT = OUT.copy()

    # Formateamos las lineas para que cada una sea una lista de tokens.
    self.lines = []
    # Funcion que representa al nodo
    self.F_comp = []
    for line in lines:
      line = [t for t in line.split(' ') if t != '']

      # Eliminamos las etiquetas.
      if line[1] == ':':
        self.lines.append(line[2:])
      else:
        self.lines.append(line)

      # Agregamos la funcion correspondiente
      if self.lines[-1][0] == 'advance': self.F_comp.append(self.f_advance)
      elif len(self.lines[-1]) == 5:
        if self.lines[-1][3] == '+': self.F_comp.append(self.f_add)
        elif self.lines[-1][3] == '-': self.F_comp.append(self.f_sub)
        elif self.lines[-1][3] == '*': self.F_comp.append(self.f_mul)
        elif self.lines[-1][3] == '/': self.F_comp.append(self.f_div)
        else: self.F_comp.append(self.f_s)
      else: self.F_comp.append(self.f_s)

  def __str__(self) -> str:
    string = f'B{self.id}:\n    '
    for line in self.lines:
      string += ' '.join(line) + '\n    '
    return string

  def F(self, X: set, warning: bool=False) -> bool:
    """
      Aplicamos la composicion de todas las funciones.
    """
    self.IN = X.copy()
    last_OUT = self.OUT.copy()
    self.OUT = X.copy()
    for i, f in enumerate(self.F_comp): f(self.OUT, self.lines[i], i, warning)
    return last_OUT != self.OUT

  def f_advance(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_advance_u(X) = X - {u}
    """
    X.discard(line[1])

  def f_add(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_u:=v+w(X) = X + {u}   si  (v in X OR v == 0) AND (w in X OR w == 0)
      f_u:=v+w(X) = X - {u}   en caso contrario
    """
    u, v, w = line[0], line[2], line[4]
    
    if (v in X or v == '0') and (w in X or w == '0'):
      X.add(u)
    else:
      X.discard(u)

  def f_mul(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_u:=v*w(X) = X + {u}   si  v in X OR v == 0 OR w in X OR w == 0
      f_u:=v*w(X) = X - {u}   en caso contrario
    """
    u, v, w = line[0], line[2], line[4]
    
    if v in X or v == '0' or w in X or w == '0':
      X.add(u)
    else:
      X.discard(u)

  def f_sub(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_u:=v-w(X) = X - {u}   si   v not in X AND v != 0 AND w == 0
      f_u:=v-w(X) = X         en caso contrario
    """
    u, v, w = line[0], line[2], line[4]

    if v not in X and v != '0' and w == '0':
      X.discard(u)
    else:
      X.add(u)

  def f_div(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_u:=v/w(X) = X - {u}   si  w == 0
      f_u:=v/w(X) = X + {u}   en caso contrario
    """
    u, w = line[0], line[4]
    if warning and w in X:
      print(f'\033[1;33mWarning\033[0m:{self.leader + l}: Potencial division by zero.')

    if w == '0':
      X.discard(u)
    else:
      X.add(u)

  def f_s(self, X: set, line: List[str], l: int, warning: bool):
    """
      f_s(X) = X
    """

class FlowGraph:
  """
    Implementacion del grafo de flujo.
  """
  def __init__(self, filename: str, labels: dict, leaders: set, vars: set):
    leaders.discard(1)
    leaders = list(leaders)
    leaders.sort()
    self.vars = vars

    with open(filename, 'r') as file:
      text = file.read()

      # Eliminamos tabuladores
      text = text.replace('\t', '')
      # Agregamos espacio entre cada token reservado
      for r in reserved:
        text = text.replace(r, f' {r} ')
      text = text.replace(': =', ':=')

      # Obtenemos las lineas
      lines = text.split('\n')

      # Creamos los nodos del grafo.
      self.V = {}
      count = 0
      last_leader = 0
      for leader in leaders:
        # El nodo estara conformado por las lineas desde el ultimo lider hasta el nuevo.
        # Su ID sera el count actual.
        self.V[count] = FlowGraphNode(lines[last_leader: leader-1], count, last_leader+1, vars)
        last_leader = leader-1
        count += 1
      if last_leader < len(lines):
        self.V[count] = FlowGraphNode(lines[last_leader:], count, last_leader+1, vars)

      # Creamos las listas de predecesores
      self.E = {}
      for n in self.V:
        last_instr = self.V[n].lines[-1]

        # Si la ultima instruccion es goto, creamos una relacion hacia el nodo destino.
        if last_instr[0] == 'goto': 
          dst = labels[last_instr[1]]
          end = False

          for id, l in enumerate(leaders):
            if dst < l: 
              if id in self.E: self.E[id].add(n)
              else: self.E[id] = {n}

              self.V[n].lines[-1][1] = f'B{id}'
              end = True
              break

          if not end: 
            id = len(leaders)
            if id in self.E: self.E[id].add(n)
            else: self.E[id] = {n}
            self.V[n].lines[-1][1] = f'B{id}'

        # En cambio, si es goif, creamos una relacion hacia el nodo destino y el nodo 
        # siguiente
        elif last_instr[0] == 'goif':
          dst = labels[last_instr[4]]
          end = False 
          for id, l in enumerate(leaders):
            if dst < l: 
              if id in self.E: self.E[id].add(n)
              else: self.E[id] = {n}

              if n+1 in self.E: self.E[n+1].add(n)
              else: self.E[n+1] = {n}

              self.V[n].lines[-1][4] = f'B{id}'
              end = True
              break
          
          if not end: 
            id = len(leaders)
            if id in self.E: self.E[id].add(n)
            else: self.E[id] = {n}
            self.V[n].lines[-1][1] = f'B{id}'

        # En caso contrario, solo creamos una relacion hacia el nodo siguiente
        else:
          if n+1 in self.E: self.E[n+1].add(n)
          else: self.E[n+1] = {n}

  def __str__(self) -> str:
    string = ''

    size = len(str(self.V[len(self.V)-1].leader + len(self.V[len(self.V)-1].lines)))

    for v in self.V: 
      string += f'\033[1;3mB{v}:\033[0m\n'

      string += '    PREDS: ' + (v == 0)*'ENTRY '
      if v in self.E:
        for u in self.E[v]: string += f'B{u} '

      if len(self.V[v].IN) > 0:
        string += f'\n    IN: {self.V[v].IN}\n'
      else:
        string += '\n    IN: {}\n'

      string += '    BLOCK:\n'
      for i, line in enumerate(self.V[v].lines):
        line_number = str(self.V[v].leader + i)
        line_number = (size - len(line_number)) * ' ' + line_number
        string += f'       {line_number}|  ' + ' '.join(line) + '\n'

      if len(self.V[v].OUT) > 0:
        string += f'\n    OUT: {self.V[v].OUT}\n\n'
      else:
        string += '\n    OUT: {}\n\n'

    return string

  def compute(self):
    change = True 

    while change:
      change = False 

      for v in self.V:
        # Si el nodo no tiene predecesores, su IN seran todas las variables.
        if v == 0 or v not in self.E: 
          change = change or self.V[v].F(self.vars)

        # En caso contrario, sera la union de los OUTS de sus predecesores.
        else:
          IN = set()
          for u in self.E[v]: IN = IN.union(self.V[u].OUT)
          change = change or self.V[v].F(IN)

  def get_warnings(self):
    """
      Una vez calculadas las variables que potencialmente pueden ser cero, generamos los 
      warnings para cada division que corresponda.
    """
    l = 1
    for v in self.V:
      self.V[v].F(self.V[v].IN, True)

if __name__ == '__main__':
  if len(argv) < 2 or len(argv) > 3:
    print("""
      Invalid syntax. Usage:
          \033[1mpython3 NaturalNumbersTAC.py\033[0m [--verbose|-v] \033[4mFILE\033[0m
    """)
    exit(1)

  elif len(argv) == 3 and argv[1] != '--verbose' and argv[1] != '-v':
    print("""
      Invalid syntax. Usage:
          \033[1mpython3 NaturalNumbersTAC.py\033[0m [--verbose|-v] \033[4mFILE\033[0m
    """)
    exit(1)
  
  elif len(argv) == 3:
    verbose = True 
    filename = argv[2]

  else:
    verbose = False
    filename = argv[1]

  # Parseamos el archivo.
  p = Parser()
  parse, labels, leaders, vars = p.parse(filename)
  if not parse: exit(0)

  # Creamos el grafo de flujo.
  fg = FlowGraph(filename, labels, leaders, vars)

  # Realizamos el analisis de flujo.
  fg.compute()

  # Obtenemos los warnings correspondientes por potencial division indefinida.
  fg.get_warnings()

  if verbose:
    print(f'\n{fg}')