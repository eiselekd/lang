import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import Comp0 from './components/Comp0';

class App extends Component {
  render() {
      return (
          <div className="App">
            <header className="App-header">
              <img src={logo} className="App-logo" alt="logo" />
              <p>
                comp: <Comp0 name={"test"} />
                Edit <code>src/App.js</code> and save to reload.
              </p>
              <a
                className="App-link"
                href="https://reactjs.org"
                target="_blank"
                rel="noopener noreferrer"
                >
                Learn React
              </a>
            </header>
          </div>
      );
  }
}

export default App;
