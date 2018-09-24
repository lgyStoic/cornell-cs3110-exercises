type student = {
  first_name: string;
  last_name: string;
  gpa: float
}

let student = {
  first_name = "John";
  last_name = "Galt";
  gpa = 10.0;
}

let get_full_name { first_name; last_name } = (first_name, last_name)

let create_student ~first_name ~last_name ~gpa = {
  first_name;
  last_name;
  gpa
}
