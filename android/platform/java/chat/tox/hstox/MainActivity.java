package chat.tox.hstox;

import android.Manifest;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import me.jappie.hatter.HatterActivity;

public class MainActivity extends HatterActivity {
    // Above the ids Hatter gives its own requests.
    private static final int NOTIFICATIONS_REQUEST = 0x7f00;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU
            && checkSelfPermission(Manifest.permission.POST_NOTIFICATIONS)
                != PackageManager.PERMISSION_GRANTED) {
            requestPermissions(
                new String[] {Manifest.permission.POST_NOTIFICATIONS}, NOTIFICATIONS_REQUEST);
        }
        startForegroundService(new Intent(this, ToxService.class));
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        if (requestCode != NOTIFICATIONS_REQUEST) {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        } else if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
            // Its notification, posted before we were allowed to, stays hidden until posted again.
            startForegroundService(new Intent(this, ToxService.class));
        }
    }
}
