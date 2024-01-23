pipeline {
    agent any
    parameters {
        string(name: 'change_type', defaultValue: '', description: 'Specify change type')
    }

    stages {
        stage('Check Git Repository') {
            steps {
                script {
                    // Define the Git repository URL
                    def gitRepoUrl = 'https://github.com/MaxCarrington/COMP390.git'

                    // Check if the Git repository exists
                    def gitRepoExists = sh(script: "git ls-remote ${gitRepoUrl}", returnStatus: true) == 0

                    if (!gitRepoExists) {
                        error("Git repository does not exist or cannot be accessed.")
                    }
                }
            }
        }
        stage('Run Strategy') {
            matrix {
                axes {
                    axis {
                        name 'strategy'
                        values 'momentum', 'market_making', 'mean_reversion', 'fixed'
                    }
                }
                stages {
                    stage('Set Results Directory') {
                        steps {
                            script {
                                def results_directory_path = ''
                                switch (strategy) {
                                    case 'momentum':
                                        results_directory_path = '/Users/maxcarrington/Documents/COMP390/Jenkins/Results/Momentum'
                                        break
                                    case 'market_making':
                                        results_directory_path = '/Users/maxcarrington/Documents/COMP390/Jenkins/Results/Market_Making'
                                        break
                                    case 'mean_reversion':
                                        results_directory_path = '/Users/maxcarrington/Documents/COMP390/Jenkins/Results/Mean_Reversion'
                                        break
                                    case 'fixed':
                                        results_directory_path = '/Users/maxcarrington/Documents/COMP390/Jenkins/Results/Fixed'
                                        break
                                    default:
                                        error("Invalid strategy: ${strategy}")
                                }
                                build job: 'Run strategy', parameters: [
                                    string(name: 'strategy', value: strategy),
                                    string(name: 'change_type', value: params.change_type),
                                    string(name: 'results_directory_path', value: results_directory_path)
                                ]
                            }
                        }
                    }
                }
            }
        }
    }
}
