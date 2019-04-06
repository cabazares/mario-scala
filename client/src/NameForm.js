import React from 'react'
import './NameForm.css'


class NameForm extends React.Component {
  constructor(props) {
    super(props);
    const { value } = this.props;
    this.state = { value: value || '' };

    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleChange(event) {
    this.setState({value: event.target.value});
  }

  handleSubmit(event) {
    const { onSubmit } = this.props;
    const { value } = this.state
    onSubmit && value && onSubmit(value)
    event.preventDefault();
  }

  render() {
    const { value } = this.state;
    return (
      <div className="NameForm">
        <form onSubmit={this.handleSubmit}>
          <label>
            <span>Name: </span> 
            <input type="text" value={value} onChange={this.handleChange} />
          </label>
          <br />
          <input type="submit" value="Play" />
        </form>
      </div>
    );
  }
}
export default NameForm
