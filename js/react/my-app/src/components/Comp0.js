import React, { Component } from 'react';

// Create a component named MessageComponent
class Comp0 extends React.Component {
  render() {
    return (
        <button className="square" onClick={ function () { console.log("hello"); } }>
          name: {this.props.name}
        </button>
    );
  }
}

export default Comp0;
