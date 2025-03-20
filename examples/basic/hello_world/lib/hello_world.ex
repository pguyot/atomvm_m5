defmodule HelloWorld do
  import Bitwise

  @moduledoc """
  Example of using M5Stack features in Elixir.
  This example demonstrates various M5Stack capabilities including:
  - Display control
  - Speaker functionality
  - RTC (Real-Time Clock)
  - Button handling
  - IMU (Inertial Measurement Unit) readings
  - Battery monitoring
  """

  @tft_white 0xFFFFFF
  @tft_cyan 0x00FFFF
  @tft_red 0xFF0000
  @tft_yellow 0xFFFF00
  @tft_blue 0x0000FF
  @tft_green 0x00FF00

  def start do
    # Initialize display
    :m5.begin_(clear_display: true)

    # Handle speaker functionality
    if :m5_speaker.is_enabled() do
      # Set master volume (0~255)
      :m5_speaker.set_volume(64)
      # Play beep sound 2000Hz 100msec (background task)
      :m5_speaker.tone(2000, 100)
      wait_until_speaker_is_done()
      # Play beep sound 1000Hz 100msec (background task)
      :m5_speaker.tone(1000, 100)
      wait_until_speaker_is_done()
      # Play raw audio
      :m5_speaker.play_raw_u8(wav_8bit_44100_binary(), 44100, false)
    end

    # Handle RTC functionality
    if :m5_rtc.is_enabled() do
      # RTC setup (commented out as it's optional)
      # :m5_rtc.set_datetime({{2021, 12, 31}, {12, 34, 56}})
    else
      IO.puts("RTC is not enabled")
    end

    # Display setup
    :m5_display.set_epd_mode(:fastest)
    :m5_display.set_brightness(128)

    # Handle display rotation for landscape mode
    if :m5_display.width() < :m5_display.height() do
      :m5_display.set_rotation(:m5_display.get_rotation() |> bxor(1))
    end

    # Set text size based on display height
    text_size0 = floor(:m5_display.height() / 160)
    text_size = max(1, text_size0)
    :m5_display.set_text_size(text_size)

    # Display initial information
    :m5_display.start_write()
    name = Atom.to_string(:m5.get_board())
    :m5_display.print("Core:")
    :m5_display.println(name)

    imu_name = Atom.to_string(:m5_imu.get_type())
    :m5_display.print("IMU:")
    :m5_display.println(imu_name)
    :m5_display.end_write()

    # Start main loop
    loop(System.monotonic_time(:second), nil, [0, 0, 0, 0, 0, 0])
  end

  defp wait_until_speaker_is_done do
    if :m5_speaker.is_playing() do
      Process.sleep(1)
      wait_until_speaker_is_done()
    end
  end

  defp loop(second, battery, x_pos_previous) do
    # Update M5Stack state
    Process.sleep(10)
    :m5.update()

    # Handle button A press/release for display sleep/wake
    if :m5_btn_a.was_pressed(), do: :m5_display.sleep()
    if :m5_btn_a.was_released(), do: :m5_display.wakeup()

    # Display button states
    # :m5_display.start_write()
    # display_button_state(:m5_btn_pwr, "pwr", 2, 783.991)
    # display_button_state(:m5_btn_a, "a", 3, 523.251)
    # display_button_state(:m5_btn_b, "b", 4, 587.330)
    # display_button_state(:m5_btn_c, "c", 5, 659.255)
    # display_button_state(:m5_btn_ext, "ext", 6, 698.456)
    # :m5_display.end_write()

    new_sec = System.monotonic_time(:second)

    {new_battery, new_x_pos_previous} =
      if new_sec == second do
        {battery, x_pos_previous}
      else
        # Update battery level
        new_battery = :m5_power.get_battery_level()

        if new_battery != battery do
          :m5_display.start_write()
          :m5_display.set_cursor(0, :m5_display.font_height() * 3)
          :m5_display.print("Bat:")

          if new_battery >= 0 do
            :m5_display.print(Integer.to_string(new_battery))
          else
            :m5_display.print("none")
          end

          :m5_display.end_write()
        end

        # Update RTC display
        if :m5_rtc.is_enabled() do
          {{year, month, day}, {hour, min, sec}} = :m5_rtc.get_datetime()
          :m5_display.start_write()
          IO.puts("Date: #{year}/#{month}/#{day}")

          :m5_display.draw_string(
            "#{year}/#{month}/#{day}",
            div(:m5_display.width(), 2),
            0
          )

          IO.puts("Time: #{hour}:#{min}:#{sec}")

          :m5_display.draw_string(
            "#{hour}:#{min}:#{sec}",
            div(:m5_display.width(), 2),
            :m5_display.font_height()
          )

          :m5_display.end_write()
        end

        # Update IMU display
        new_x_pos_previous = 0

        new_x_pos_previous =
          if :m5_imu.is_enabled() do
            h = div(:m5_display.height(), 8)
            {_updated_a, {ax, ay, az}} = :m5_imu.get_accel()
            {_updated_g, {gx, gy, gz}} = :m5_imu.get_gyro()

            x_pos = [
              floor(ax * 50),
              floor(ay * 50),
              floor(az * 50),
              floor(gx / 2),
              floor(gy / 2),
              floor(gz / 2)
            ]

            :m5_display.start_write()
            :m5_display.set_clip_rect(h, h, :m5_display.width(), :m5_display.height())
            :m5_display.wait_display()
            colors = [@tft_red, @tft_green, @tft_blue, @tft_red, @tft_green, @tft_blue]
            display_imu(x_pos, x_pos_previous, colors, 0)
            :m5_display.clear_clip_rect()
            :m5_display.end_write()
            x_pos
          else
            x_pos_previous
          end

        {new_battery, new_x_pos_previous}
      end

    loop(new_sec, new_battery, new_x_pos_previous)
  end

  defp display_imu([], [], [], 6), do: :ok

  defp display_imu([x_pos | t], [x_pos | t_previous], [_color | colors_t], i) do
    display_imu(t, t_previous, colors_t, i + 1)
  end

  defp display_imu([x_pos | t], [px | t_previous], [color | colors_t], i) do
    h = div(:m5_display.height(), 8)
    ox = div(:m5_display.width() + h, 2)

    new_px =
      if x_pos < 0 != px < 0 do
        if px != 0 do
          :m5_display.fill_rect(ox, h * (i + 2), px, h, :m5_display.get_base_color())
          0
        else
          0
        end
      else
        px
      end

    if x_pos != new_px do
      if x_pos > new_px != x_pos < 0 do
        :m5_display.set_color(color)
      else
        :m5_display.set_color(:m5_display.get_base_color())
      end

      :m5_display.fill_rect(x_pos + ox, h * (i + 2), new_px - x_pos, h)
    end

    display_imu(t, t_previous, colors_t, i + 1)
  end

  defp display_button_state(module, name, index, freq) do
    case button_state_name(module) do
      :none ->
        :ok

      state_name ->
        :m5_speaker.tone(freq, 100)
        IO.puts("#{name} state: #{state_name} #{module.get_click_count()}")

        if not :m5_display.display_busy() do
          state_colors = %{
            "none" => @tft_white,
            "was_hold" => @tft_cyan,
            "was_clicked" => @tft_red,
            "was_pressed" => @tft_yellow,
            "was_released" => @tft_blue,
            "was_decide_click_count" => @tft_green
          }

          h = div(:m5_display.height(), 8)
          :m5_display.fill_rect(0, h * index, h, h - 1, Map.get(state_colors, state_name))
          :m5_display.set_cursor(0, h * index)
          :m5_display.print(Integer.to_string(module.get_click_count()))
        end
    end
  end

  defp button_state_name(button_module) do
    button_state_name(button_module, [
      :was_hold,
      :was_clicked,
      :was_pressed,
      :was_released,
      :was_decide_click_count
    ])
  end

  defp button_state_name(button_module, [test | tail]) do
    if button_module.test() do
      Atom.to_string(test)
    else
      button_state_name(button_module, tail)
    end
  end

  defp button_state_name(_button_module, []), do: :none

  # This is a placeholder for the WAV binary data
  defp wav_8bit_44100_binary do
    # You would need to provide the actual WAV binary data here
    <<>>
  end
end
