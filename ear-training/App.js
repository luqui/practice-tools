import React from 'react';
import { StyleSheet, Text, View, TouchableWithoutFeedback } from 'react-native';
import { Audio } from 'expo-av';
import Svg, {Rect} from 'react-native-svg';

export default function App() {
  return (
    <View style={styles.container}>
      <Text>Open up App.js to start working on your app!</Text>
      <Keyboard />
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'center',
  },
});

export class Keyboard extends React.Component {
  constructor() {
    super();
    this.state = {};
    console.log("constructor", this);
  }

  componentDidMount() {
    console.log("mounting", this);
    let synth = new Synth();
    synth.initAsync();
    this.setState(oldState => {
      console.log("Setting state (old state", this.state, ") or (", oldState, ")");
      return { synth: synth };
    });
    console.log("Still in didMount");
  }
    
  _pressIn() {
    console.log("this = ", this);
    this.state.synth.noteOn(60);
  }
  
  _pressOut() {
    this.state.synth.noteOff(60);
  }

  render() {
    return (
      <TouchableWithoutFeedback onPressIn={this._pressIn.bind(this)} onPressOut={this._pressOut.bind(this)}>
       <Svg height="300" width="300" viewBox="0 0 100 100">
        <Rect x={50} y={50} width={50} height={50} stroke="blue" strokeWidth="2.5" fill="green" />
       </Svg>
      </TouchableWithoutFeedback>
    );
  }
}

export class Synth  {
  async initAsync() {
    this.audios = [];
    let loadNote = async (mod) => {
      const audiofile = mod;
      const cur = new Audio.Sound();
      await cur.loadAsync(audiofile);
      console.log("Loading", audiofile);
      this.audios.push(cur);
    }

    await Promise.all([
      loadNote(require('./assets/piano-note36-2sec.mp3')),
      loadNote(require('./assets/piano-note37-2sec.mp3')),
      loadNote(require('./assets/piano-note38-2sec.mp3')),
      loadNote(require('./assets/piano-note39-2sec.mp3')),
      loadNote(require('./assets/piano-note40-2sec.mp3')),
      loadNote(require('./assets/piano-note41-2sec.mp3')),
      loadNote(require('./assets/piano-note42-2sec.mp3')),
      loadNote(require('./assets/piano-note43-2sec.mp3')),
      loadNote(require('./assets/piano-note44-2sec.mp3')),
      loadNote(require('./assets/piano-note45-2sec.mp3')),
      loadNote(require('./assets/piano-note46-2sec.mp3')),
      loadNote(require('./assets/piano-note47-2sec.mp3')),
                                                         
      loadNote(require('./assets/piano-note48-2sec.mp3')),
      loadNote(require('./assets/piano-note49-2sec.mp3')),
      loadNote(require('./assets/piano-note50-2sec.mp3')),
      loadNote(require('./assets/piano-note51-2sec.mp3')),
      loadNote(require('./assets/piano-note52-2sec.mp3')),
      loadNote(require('./assets/piano-note53-2sec.mp3')),
      loadNote(require('./assets/piano-note54-2sec.mp3')),
      loadNote(require('./assets/piano-note55-2sec.mp3')),
      loadNote(require('./assets/piano-note56-2sec.mp3')),
      loadNote(require('./assets/piano-note57-2sec.mp3')),
      loadNote(require('./assets/piano-note58-2sec.mp3')),
      loadNote(require('./assets/piano-note59-2sec.mp3')),
                                                         
      loadNote(require('./assets/piano-note60-2sec.mp3')),
      loadNote(require('./assets/piano-note61-2sec.mp3')),
      loadNote(require('./assets/piano-note62-2sec.mp3')),
      loadNote(require('./assets/piano-note63-2sec.mp3')),
      loadNote(require('./assets/piano-note64-2sec.mp3')),
      loadNote(require('./assets/piano-note65-2sec.mp3')),
      loadNote(require('./assets/piano-note66-2sec.mp3')),
      loadNote(require('./assets/piano-note67-2sec.mp3')),
      loadNote(require('./assets/piano-note68-2sec.mp3')),
      loadNote(require('./assets/piano-note69-2sec.mp3')),
      loadNote(require('./assets/piano-note70-2sec.mp3')),
      loadNote(require('./assets/piano-note71-2sec.mp3')),
                                                         
      loadNote(require('./assets/piano-note72-2sec.mp3')),
      loadNote(require('./assets/piano-note73-2sec.mp3')),
      loadNote(require('./assets/piano-note74-2sec.mp3')),
      loadNote(require('./assets/piano-note75-2sec.mp3')),
      loadNote(require('./assets/piano-note76-2sec.mp3')),
      loadNote(require('./assets/piano-note77-2sec.mp3')),
      loadNote(require('./assets/piano-note78-2sec.mp3')),
      loadNote(require('./assets/piano-note79-2sec.mp3')),
      loadNote(require('./assets/piano-note80-2sec.mp3')),
      loadNote(require('./assets/piano-note81-2sec.mp3')),
      loadNote(require('./assets/piano-note82-2sec.mp3')),
      loadNote(require('./assets/piano-note83-2sec.mp3')),
                                                         
      loadNote(require('./assets/piano-note84-2sec.mp3')),
    ]);
  }

  async noteOn(note, _vel) {
    let noteid = note - 36;
    await this.audios[noteid].setPositionAsync(0);
    await this.audios[noteid].playAsync();
  }

  async noteOff(note) {
    let noteid = note - 36;
    await this.audios[noteid].stopAsync();
  }
}
