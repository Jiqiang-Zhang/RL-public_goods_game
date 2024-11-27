module agent_definitions
    use iso_fortran_env, only: real64
    implicit none
    type :: agent
        real(kind=real64), dimension(0:1, 0:4, 0:1) :: Q_table
        ! Q_table: 3D array for Q-learning value function
        ! The cognitive action values in different states: 
        ! - Dimension 1 (0:1): the agent's previous action (0 or 1) in the corresponding hyperedge
        ! - Dimension 2 (0:4): the number of cooperators in the hyperedge except itself
        ! the above dimesions denoting the different states
        ! - Dimension 3 (0:1): the action (0 or 1) in different state
        integer, dimension(0:4, 1:2) :: state
        ! state: 2D array representing the agent's interaction with hyperedges
        ! - Dimension 1 (0:4): Hyperedge labels
        !   - 0: The agent's own corresponding hyperedge
        !   - 1 to 4: The agent's four nearest hyperedges
        ! - Dimension 2 (1:2): Contains information about the agent's actions and neighbors in the hyperedges
        !   - 1: The agent's previous action in the hyperedge (0 or 1)
        !   - 2: The number of cooperators among the agent's neighbors in the hyperedge (0 to 4)
        integer, dimension(0:4) :: act
        ! act: Array mark the agent's next action in all hyperedges including it
        ! - Dimension 0: The agent's action serving as the inititor
        ! - Dimensions 1 to 4: The agent's actions serving as participants in different hypedges
        real(kind=real64) :: reward
        ! reward: The reward the agent received serving as the initiator
    end type agent

    contains

    subroutine pay(numbers, synergy_factor_r, payoff_table)
        implicit none

        ! Input variables
        integer, intent(in) :: numbers  ! The number of participants in the public goods game.
        real(kind=real64), intent(in) :: synergy_factor_r! The synergy_factor without normalization
        ! Output variable
        real(kind=real64), dimension(0:1, 0:(numbers-1)), intent(out) :: payoff_table
        ! The payoff matrix, where:
        ! - First index (0 or 1): the agent's action (0: defection, 1: cooperation).
        ! - Second index (0 to numbers-1): the number of cooperators in its hyperedge.
        ! The table stores the corresponding payoff in each scenario.
        integer :: i, j  
        do j = 0, (numbers-1)  
            do i = 0, 1  
                payoff_table(i, j) = (real(j + i, kind=real64) * synergy_factor_r / real(numbers, kind=real64)) - &
                real(i, kind=real64)
            end do
        end do

    end subroutine pay

end module agent_definitions

module networks
    implicit none

    contains
    ! This subroutine creates a lattice structure for agents in a grid-like network.
    ! - Each agent is connected to its four nearest neighbors: top (up), bottom, left, and right.
    ! - The grid is structured such that the agents' positions correspond to specific hyperedges,
    !   and connections wrap around when agents are located at the borders of the grid.
    subroutine lattice(v, num, x)
        implicit none
        integer, intent(in) :: num, x  ! num: total number of agents, x: the lengh of the square
        integer, dimension(num, 4), intent(out) :: v  ! Array holding the four nearest neighbor connections for each agent
        integer :: i  ! Loop variable

        ! Initialize the neighbor list with zeros
        v = 0

        do i = 1, num

            ! Bottom neighbor: the agent located directly below in the grid (hyperedge beneath)
            if (i - x > 0) then
                v(i, 1) = i - x  ! Bottom neighbor
                v(i - x, 2) = i  ! Up neighbor (corresponding agent above in the hyperedge)
            else
                v(i, 1) = i + num - x  ! Bottom neighbor with wrap-around at the grid edge
                v(i + num - x, 2) = i  ! Up neighbor with wrap-around at the grid edge
            end if

            ! Calculate the left and right neighbors for the agent in its hyperedge

            ! Left neighbor: the agent located directly to the left (hyperedge to the left)
            if (mod(i, x) == 1) then
                v(i, 3) = i + x - 1  ! Left neighbor with wrap-around at the edge of the row
                v(i + x - 1, 4) = i  ! Right neighbor (corresponding agent to the right)
            else
                v(i, 3) = i - 1  ! Left neighbor (directly to the left in the same row)
                v(i - 1, 4) = i  ! Right neighbor (directly to the right in the same row)
            end if
        end do

    end subroutine lattice

end module networks


module initialization
    use agent_definitions  ! Import the agent definitions module.
    implicit none

    contains

    ! Initialize agents' attributes and their network interactions.
    ! This subroutine initializes the Q-tables, rewards, and action history for each agent.
    subroutine initialize(nodes, num, v)
        implicit none
        integer, intent(in) :: num  ! Number of agents.
        type(agent), dimension(num), intent(out) :: nodes  ! Array of agents, containing all agents' information.
        integer, dimension(num, 4), intent(in) :: v  ! Neighbors' indices for each agent.

        ! Temporary arrays for random numbers used in initialization.
        real(kind=real64), dimension(0:1, 0:4, 0:1) :: ran1  ! Random numbers for Q_table initialization.
        real(kind=real64), dimension(0:4) :: ran2  ! Random numbers for action selection.
        integer :: i, j  ! Loop variables.

        ! Generate random numbers for Q-table initialization.
        do i = 1, num
            ! Initialize Q-table, rewards, and actions based on random numbers.
            call random_number(ran1)               
            call random_number(ran2)

            nodes(i)%Q_table = (ran1 - 0.5) * 0.01  ! Q-table values range from -0.005 to 0.005.
            nodes(i)%reward = 0.0  ! Initialize reward to 0.
            nodes(i)%act = 0  ! Initialize action to 0.

            ! Generate initial actions based on random numbers.
            do j = 0, 4
                if (ran2(j) < 0.5) then
                    nodes(i)%state(j, 1) = 0  ! Set state to 0, indicating non-cooperation.
                    nodes(i)%act(j) = 0  ! Set action to 0, indicating non-cooperation.
                else
                    nodes(i)%state(j, 1) = 1  ! Set state to 1, indicating cooperation.
                    nodes(i)%act(j) = 1  ! Set action to 1, indicating cooperation.
                endif
            end do
        end do

        ! The number of contributors in participants when the agent serve as the inititor.
        do i = 1, num
            nodes(i)%state(0, 2) = nodes(v(i, 1))%state(2, 1) + nodes(v(i, 2))%state(1, 1) + &
                                   nodes(v(i, 3))%state(4, 1) + nodes(v(i, 4))%state(3, 1)
        end do

        ! The number of contributors in the corresponsing hyper-edges when the agent serves as the participants.
        do i = 1, num
            do j = 1, 4
                nodes(i)%state(j, 2) = nodes(v(i, j))%state(0, 2) + nodes(v(i, j))%state(0, 1) - nodes(i)%state(j, 1)
            end do
        end do
    end subroutine

end module initialization


module update
    use agent_definitions

    contains

    subroutine updating(nodes, num, alpha, gamma, epsilons, payoff_table, v, numbers)
        ! Update the states and decisions of agents based on their interactions and learning parameters.
        ! This subroutine modifies each agent's action and updates their Q-table based on exploration or exploitation strategies.
        implicit none
        integer :: num  ! Number of agents.
        type(agent), dimension(num) :: nodes  ! agent set
        integer :: i, j, m, numbers  !cycling variables.
        real(kind=real64) :: alpha  ! Learning rate
        real(kind=real64) :: gamma  ! Discount factor
        real(kind=real64) :: epsilons  ! Exploration rate
        real(kind=real64) :: random1, t, TD  ! Temporary variables for random number generation and calculations.
        integer, dimension(num, 4) :: v  ! adjacency matrix
        real(kind=real64), dimension(0:1, 0:(numbers-1)) :: payoff_table  ! Payoff_table 

        ! Iterate through each agent to update its actions based on exploration or exploitation.
        do i = 1, num
            do j=0, 4
                call random_number(random1)  
                !random action under exploration
                if (random1 < epsilons) then  
                    call random_number(random1)  
                    if (random1 < 0.5) then
                        nodes(i)%act(j) = 0  
                    else
                        nodes(i)%act(j) = 1  
                    endif
                !take the action ased on Q-table values.
                else  
                    if (nodes(i)%Q_table(nodes(i)%state(j,1), nodes(i)%state(j,2), 0) > &
                        nodes(i)%Q_table(nodes(i)%state(j,1), nodes(i)%state(j,2), 1)) then
                        nodes(i)%act(j) = 0  
                    else
                        nodes(i)%act(j) = 1  
                    endif
                endif
            end do
            ! Update actions for each agent based on exploration or exploitation.
        end do

        ! Update rewards and Q-table for each agent based on the new state and actions.
        do i = 1, num
            m = nodes(v(i, 1))%act(2) + nodes(v(i, 2))%act(1) +&
                nodes(v(i, 3))%act(4) + nodes(v(i, 4))%act(3)
            ! Calculate the reward for agent i based on its action and the actions of its neighbors.
            nodes(i)%reward = payoff_table(nodes(i)%act(0), m)

            ! Determine the maximum expected future reward from the Q-table.
            if (nodes(i)%Q_table(nodes(i)%act(0), m, 0) > nodes(i)%Q_table(nodes(i)%act(0), m, 1)) then
                t = nodes(i)%Q_table(nodes(i)%act(0), m, 0)  ! Max Q-value for action 0.
            else
                t = nodes(i)%Q_table(nodes(i)%act(0), m, 1)  ! Max Q-value for action 1.
            endif

            ! Update the Q-table using the Temporal Difference (TD) learning algorithm.
            TD = alpha * (nodes(i)%reward + gamma * t - &
                          nodes(i)%Q_table(nodes(i)%state(0,1), nodes(i)%state(0,2), nodes(i)%act(0)))
            nodes(i)%Q_table(nodes(i)%state(0,1), nodes(i)%state(0,2), nodes(i)%act(0)) = &
                          nodes(i)%Q_table(nodes(i)%state(0,1), nodes(i)%state(0,2), nodes(i)%act(0)) + TD
            ! Update the actions and rewards based on the interaction with neighbors.
            nodes(i)%state(0,2) = m  ! Store the total action of neighbors in act(0,2).
            do j=0,4
                nodes(i)%state(j,1) = nodes(i)%act(j)  ! Update the agent's state based on the current action.
            end do
        end do

        ! Update the observed state for each agent based on neighbors' actions.
        do i = 1, num
            do j = 1, 4
                nodes(i)%state(j,2) = nodes(v(i,j))%state(0,2) + nodes(v(i,j))%state(0,1) - nodes(i)%state(j,1)
                ! Here, state(j,2) represents the number of cooperating agents observed by agent i
                ! when participating in the hyperedge game of its neighbor j.
            end do
        end do
    end subroutine

end module update


program main
    use agent_definitions  ! Use the module that defines the agent structure and attributes.
    use initialization      ! Use the module for initializing agents and their states.
    use networks           ! Use the module for creating and managing the network structure.
    use update             ! Use the module for updating agents' states and Q-tables.
    implicit none

    integer, parameter :: num = 900, l = 30  ! Set the total number of agents and the lattice size.
    integer, parameter :: numbers = 5        ! The total number of players in each game is set at 5.
    type(agent), dimension(:), allocatable :: nodes  ! Dynamically allocated array of agents.
    integer, parameter ::cycle_time = 1000000000

    ! Learning parameters
    real(kind=real64) :: alpha = 0.1        ! Learning rate for Q-table updates.
    real(kind=real64) :: gamm = 0.9         ! Discount factor for future rewards.
    real(kind=real64) :: epsilons = 0.01    ! Exploration rate for action selection.
    real(kind=real64) :: synergy_factor_r    ! the synergy factor without normalization
    integer, dimension(num, 4) :: v          ! Array for neighbor indices for each agent.
    real(kind=real64), dimension(0:1, 0:(numbers-1)) :: payoff_matrix  ! Payoff matrix for the game.

    ! Arrays to store levels for agents over time
    real(kind=real64):: level
    integer :: i, j, m, n, file_unit
    character(len=100) :: filename, filename1  ! Filenames for output data.
    integer :: seed1(33)  ! Seed array for random number generation.

    allocate(nodes(num))  ! Allocate memory for the array of agents.

    ! Initialize the random seed for reproducibility.
    do i = 1, 33
        seed1(i) = i + 687900  
    end do
    call random_seed(put = seed1)  ! Apply the seed for random number generation.
    call lattice(v, num, l)  ! Create the lattice structure connecting agents.

    ! Main loop for varying the parameter used in the payoff matrix.
        synergy_factor_r = 3.6  ! Increment the parameter for payoff calculations.

        ! Prepare filenames for output based on the current double parameter.
        
        write(filename1, '(f5.3)') synergy_factor_r  
        filename1 = trim(filename1) // '-' // 'time1.txt'  
        
        ! Write results to files.
        open(unit=file_unit, file=filename1, status='replace', action='write')  ! Open the file for writing.
        if (file_unit < 0) then  ! Check if the file opened successfully.
            write(*, '(A, I0)') 'Error opening file: ', file_unit  ! Print error message if file opening fails.
            stop
        end if
        call pay(numbers, synergy_factor_r, payoff_matrix)  ! Generate the payoff matrix based on the parameter.

        level = 0.0;  ! Initialize arrays for recording levels and a count variable.

        ! Initial phase of updating agents for a set number of iterations.
        call initialize(nodes, num, v)

        ! Main loop for running the simulation and recording results.
        do n = 1, cycle_time
            call updating(nodes, num, alpha, gamm, epsilons, payoff_matrix, v, numbers)  ! Continue updating agents.
            
            level=0.0
            do i=1,num
                level =level + (nodes(i)%state(0,1) + nodes(i)%state(0,2)) / 5.0  
            end do
            write(file_unit, '(f6.4)') level  
        end do
        close(file_unit) 

    deallocate(nodes)  ! Free the allocated memory for agents.
end program
