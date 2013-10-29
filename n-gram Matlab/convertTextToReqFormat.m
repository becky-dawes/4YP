function text = convertTextToReqFormat( filename )

file = fopen(filename);
fprintf(['Opening file ', filename, '\n']);
data = fread(file);
fclose(file);

lcase = abs('a'):abs('z');
ucase = abs('A'):abs('Z');
caseDiff = abs('a') - abs('A');

fprintf('Converting letters to lower case \n');

caps = ismember(data,ucase);
data(caps) = data(caps)+caseDiff;

fprintf('Removing punctuation \n');

validSet = [abs(' ') lcase];
data = data(ismember(data,validSet));


wordCount = 0;
space = false;
for i=1:length(data)
    if data(i) == abs(' ')
        if ~space
            wordCount = wordCount +1;
        end
        space = true;
    else
        space = false;
    end
end

text = cell(wordCount,1);
currentWord = 1;
newWord = true;
space = false;
for i=1:length(data)
    if data(i) == abs(' ')
        if ~space
            currentWord = currentWord +1;
            newWord = true;
        end
        space = true;
    else
        space = false;
        newAddition = char(data(i));
        if newWord
            word = newAddition;
        else
            word = text{currentWord};
            word = strcat(word,newAddition);
        end
        newWord = false;
        text{currentWord,1} = word;
    end
end

display(text{101});
display('Hello');

end

