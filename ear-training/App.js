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
    
  _pressIn(note) {
    console.log("pressIn this = ", this);
    this.state.synth.noteOn(note);
  }
  
  _pressOut(note) {
    console.log("pressOut this = ", this);
    this.state.synth.noteOff(note);
  }

  render() {
    return (
       <Svg height="300" width="300" viewBox="0 0 100 100">
        <TouchableWithoutFeedback onPressIn={this._pressIn.bind(this, 60)} onPressOut={this._pressOut.bind(this, 60)}>
          <Rect x={0} y={50} width={50} height={50} stroke="blue" strokeWidth="2.5" fill="green" />
        </TouchableWithoutFeedback>
        <TouchableWithoutFeedback onPressIn={this._pressIn.bind(this, 61)} onPressOut={this._pressOut.bind(this, 61)}>
          <Rect x={50} y={50} width={50} height={50} stroke="blue" strokeWidth="2.5" fill="red" />
        </TouchableWithoutFeedback>
       </Svg>
    );
  }
}

export class Synth  {
  async initAsync() {
    this.audios = [];
    let loadNote = async (note, mod) => {
      const audiofile = mod;
      const cur = new Audio.Sound();
      this.audios[note - 36] = cur;
      await cur.loadAsync(audiofile, {}, true);
      console.log("Loaded", audiofile);
    }

    await Promise.all([
      /*
      loadNote(36, require('./assets/piano-note36-2sec.mp3')),
      loadNote(37, require('./assets/piano-note37-2sec.mp3')),
      loadNote(38, require('./assets/piano-note38-2sec.mp3')),
      loadNote(39, require('./assets/piano-note39-2sec.mp3')),
      loadNote(40, require('./assets/piano-note40-2sec.mp3')),
      loadNote(41, require('./assets/piano-note41-2sec.mp3')),
      loadNote(42, require('./assets/piano-note42-2sec.mp3')),
      loadNote(43, require('./assets/piano-note43-2sec.mp3')),
      loadNote(44, require('./assets/piano-note44-2sec.mp3')),
      loadNote(45, require('./assets/piano-note45-2sec.mp3')),
      loadNote(46, require('./assets/piano-note46-2sec.mp3')),
      loadNote(47, require('./assets/piano-note47-2sec.mp3')),
                                                         
      loadNote(48, require('./assets/piano-note48-2sec.mp3')),
      loadNote(49, require('./assets/piano-note49-2sec.mp3')),
      loadNote(50, require('./assets/piano-note50-2sec.mp3')),
      loadNote(51, require('./assets/piano-note51-2sec.mp3')),
      loadNote(52, require('./assets/piano-note52-2sec.mp3')),
      loadNote(53, require('./assets/piano-note53-2sec.mp3')),
      loadNote(54, require('./assets/piano-note54-2sec.mp3')),
      loadNote(55, require('./assets/piano-note55-2sec.mp3')),
      loadNote(56, require('./assets/piano-note56-2sec.mp3')),
      loadNote(57, require('./assets/piano-note57-2sec.mp3')),
      loadNote(58, require('./assets/piano-note58-2sec.mp3')),
      loadNote(59, require('./assets/piano-note59-2sec.mp3')),
                                                         */
      loadNote(60, require('./assets/piano-note60-2sec.mp3')),
      loadNote(61, require('./assets/piano-note61-2sec.mp3')),
      /*
      loadNote(62, require('./assets/piano-note62-2sec.mp3')),
      loadNote(63, require('./assets/piano-note63-2sec.mp3')),
      loadNote(64, require('./assets/piano-note64-2sec.mp3')),
      loadNote(65, require('./assets/piano-note65-2sec.mp3')),
      loadNote(66, require('./assets/piano-note66-2sec.mp3')),
      loadNote(67, require('./assets/piano-note67-2sec.mp3')),
      loadNote(68, require('./assets/piano-note68-2sec.mp3')),
      loadNote(69, require('./assets/piano-note69-2sec.mp3')),
      loadNote(70, require('./assets/piano-note70-2sec.mp3')),
      loadNote(71, require('./assets/piano-note71-2sec.mp3')),
                                                         
      loadNote(72, require('./assets/piano-note72-2sec.mp3')),
      loadNote(73, require('./assets/piano-note73-2sec.mp3')),
      loadNote(74, require('./assets/piano-note74-2sec.mp3')),
      loadNote(75, require('./assets/piano-note75-2sec.mp3')),
      loadNote(76, require('./assets/piano-note76-2sec.mp3')),
      loadNote(77, require('./assets/piano-note77-2sec.mp3')),
      loadNote(78, require('./assets/piano-note78-2sec.mp3')),
      loadNote(79, require('./assets/piano-note79-2sec.mp3')),
      loadNote(80, require('./assets/piano-note80-2sec.mp3')),
      loadNote(81, require('./assets/piano-note81-2sec.mp3')),
      loadNote(82, require('./assets/piano-note82-2sec.mp3')),
      loadNote(83, require('./assets/piano-note83-2sec.mp3')),
                                                         
      loadNote(84, require('./assets/piano-note84-2sec.mp3')),*/
    ]);
  }

  async noteOn(note, _vel) {
    let noteid = note - 36;
    //await this.audios[noteid].setPositionAsync(0);
    await this.audios[noteid].replayAsync();
  }

  async noteOff(note) {
    let noteid = note - 36;
    await this.audios[noteid].stopAsync();
  }
}
