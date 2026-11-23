@0xbd528ee550dcaddf;
# Text annotations longer than a 132-column line on every target kind.

annotation doc(*) :Text;

$doc("File note: Cap'n Proto annotations carry free text, and this one is deliberately long so that the emitted Fortran parameter cannot fit on a single free-form source line. It mixes 'single quotes', \"double quotes\", tabs\tand a trailing run of punctuation;,.:!? that the splitter must keep intact across every continuation line it writes.");

struct Note $doc("Struct note: ThisSentenceHasNoSpacesAtAllSoTheSplitterCannotLeanOnWordBoundariesAndMustCutInsideOneUnbrokenRunOfCharactersThatKeepsGoingWellPastOneHundredAndThirtyTwoColumnsBeforeItFinallyStopsAndThenTheRestOfTheNoteAddsMoreWordsUntilTheWholeValueIsComfortablyOverThreeHundredCharactersLongForTheRegression.") {
  body @0 :Text $doc("Field note: a field-level annotation that is long enough to need several continuation lines in the generated module, written as ordinary prose so the value reads naturally while still running well past three hundred characters in total, which is the length this regression is meant to cover end to end.");
}

enum Tone $doc("Enum note: an enum-level annotation of the same shape, long enough that a single quoted literal would overflow column one hundred and thirty-two, which gfortran rejects under its default free-form line length unless a line-length flag is passed on the command line.") {
  low @0 $doc("Enumerant note: even an enumerant can carry a long Text annotation, and its emitted constant has to be split over continuation lines just like the others, so this value keeps going until it is safely longer than three hundred characters in all.");
}

interface Pinger $doc("Interface note: interfaces get their own annotation constants as well, and this text is long enough that the emitted parameter must be continued over several lines rather than written in one go, which is exactly the failure this regression guards against.") {
  ping @0 () -> () $doc("Method note: method annotations are emitted next to the interface, and this one is long enough that the emitted Text constant needs continuation lines to stay within the default line length that gfortran enforces on free-form source.");
}
