import { useState } from 'react';

export default function App() {
  const [textInput, setTextInput] = useState('');
  const [response, setResponse] = useState('');

  const handleTextChange = (event) => {
    setTextInput(event.target.value);
  };

  const handleExampleClick = () => {
    const exampleSExpression = '(1 (2A 2.5 2Test3 2..5 "hello" my-symbol))';
    setTextInput(exampleSExpression);
  };

  const handleSubmit = async (event) => {
    event.preventDefault();

    const requestOptions = {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain;charset=UTF-8',
      },
      body: textInput,
    };

    try {
      const response = await fetch('http://localhost:3210/convert', requestOptions);
      const json = await response.json();
      setResponse(JSON.stringify(json, null, 2));
    } catch (error) {
      console.error('Error:', error);
      setResponse('Une erreur s\'est produite lors de la requête.');
    }
  };

  return (
    <div>
      <h1>sexpr2json</h1>
      <form onSubmit={handleSubmit}>
        <div>
          <label>Entrée:</label>
          <input type="text" value={textInput} onChange={handleTextChange} />
          <button type="button" onClick={handleExampleClick}>
            Utiliser un exemple
          </button>
        </div>
        <div>
          <button type="submit">Envoyer la requête</button>
        </div>
      </form>
      <div>
        <h2>Réponse:</h2>
        <pre>{response}</pre>
      </div>
    </div>
  );
}
