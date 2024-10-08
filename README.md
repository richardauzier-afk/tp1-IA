# Primeiro trabalho de IA

Equipe: Caio Muniz,Richard Auzier

## Sobre o trabalho:

Este programa em Prolog é um planejador para o domínio do mundo dos blocos, onde blocos podem ser movidos entre posições (lugares) com base em certas regras. O mundo dos blocos é um problema clássico em inteligência artificial.
##  Como Executar passo a passo:

1. **Ambiente SWISH - Prolog:**:
   Foi utilizado o site https://swish.swi-prolog.org.
    Basta copiar e colar o código no editor de texto do site.

3. **Defina o estado inicial**:
   Você pode definir o estado inicial (state1 predicado) no arquivo Prolog com base no seu problema. Por padrão, o estado inicial é o estado inicial proposto no livro do Bratko.

4. **Definir metas**
   Você pode definir as metas que você deseja alcançar colocando os goals na lista do predicado Plan

5. **Forma de executar**:

Aqui está um exemplo de como usar o planejador para um problema específico:

```prolog        
  
   % definação do estado inicial
   state1([on(c,p([1,3])),on(a,b),on(b,p([5,5])),on(d,a),clear(c),clear(d),clear(4),clear(6)]).
  % Definição do goal
    on(c,d).
```

```prolog
   % execução do planejador

Start = [on(c,p([1,3])),on(a,b),on(b,p([5,5])),on(d,a),clear(c),clear(d),clear(4),clear(6)],
plan(Start, [on(c,d)], Plan).
```















