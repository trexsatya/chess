# Chess

## Design
#### Object-Oriented Analysis:
<b> Requirements Analysis </b>
<br><u>Core Requirements:</u>
<ul>
    <li> I can create a new game to play with my friend. </li>
    <li> We can see the board with pieces arranged properly. </li>
    <li> I get color white. White makes the first move.</li>
    <li> Black gets his turn. We make turns after each move.</li>
    <li> If white tries to move black piece, the game does not allow it. Similarly for white piece.</li>
    <li> When I select a piece, valid moves are shown to me. </li>
    <li> When I select from valid moves, pieces move appropriately and board is updated. </li>
    <li> I must be able to save and resume the game. </li>
</ul>
<br>
<u> Extra Requirements (which we can think of in advance)</u> Remember the more the vision, better the design, 
but you can quite easily get stuck in futuristic requirements. You need to find a balance. 
<br>

<ul>
    <li> I can play against the computer. </li>
    <li> I can undo my moves. </li>
    <li> I can play online with other people. </li>
    <li> The UI of the game can be enhanced with rich interface. </li>
</ul>

<b>Identify entities involved</b>
<ul>
   <li><b>Identify entities:</b>
     Some of them clearly obvious are: 
     <code>Game</code>,
     <code>Board</code>,
     <code>Square/Cell</code>,
     <code>Player</code> 
   </li>
   
   <li>
    <b> How are they related? How will they interact?</b>
    <ul> 
        <li>A <code>Game</code> will run in a loop, making turns, asking for input from the player. </li>
        <li>Or, we can make this reactive. game will receive commands from player, and if it is valid a move will be executed. </li>
        <li>Initially pieces are laid out on the board. <code> Board </code> knows which squares/<code>Cell</code>s has which piece.
        Board manages the layout of the pieces.</li>
        <li><code>Piece</code> has certain properties like color. </li>
        <li><code>Piece</code> needs to move, and for a valid move it needs to have access to <code>Board</code> </li>
        <li>Different types of pieces have different rules for moves </li>
        <li>A <code>Piece</code> must be able to figure out where it can go, given access to the <code>Board</code></li>
        <li>There are certain moves which piece cannot figure out just by looking at the board, e.g. En-Passant which needs to know what was the last move </li>
        <li>We can clearly see that moves should be stored for various purpose (undoing, validating en-passe etc). 
        Hence we need to model <code>Move</code>
        </li>        
    </ul>
   </li> 
</ul>

    
