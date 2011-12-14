for i in {0..9999}
do
  num=`printf "%04d" $i`
  echo $num
  7z e -yp"$num" "$1" > /dev/null
  if [ $? -eq 0 ]
  then
    echo "Password is $num."
    exit 0
  fi
done

