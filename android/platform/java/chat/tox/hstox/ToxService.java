package chat.tox.hstox;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import me.jappie.hatter.HatterCallAudio;
import me.jappie.hatter.HatterNotifications;

/**
 * Keeps the process, and with it Tox, running while no screen is open, and
 * a call's microphone and camera while it goes on.
 */
public class ToxService extends Service {
    static {
        // When Android restarts only the service, this starts Tox.
        System.loadLibrary("hatter");
    }

    private static final String CHANNEL = "connection";
    private static final int NOTIFICATION = 1;
    /** Whether a call's audio runs. */
    private volatile boolean calling;

    @Override
    public void onCreate() {
        super.onCreate();
        HatterCallAudio.setListener((context, running) -> {
            calling = running;
            foreground(running);
        });
        HatterNotifications.setOngoingListener(context -> foreground(calling));
        // With stay online off once hTox is closed: Tox goes with the process,
        // and starts again with it when hTox is opened.
        HatterNotifications.setStopListener(context -> new Handler(Looper.getMainLooper()).post(() -> {
            stopForeground(STOP_FOREGROUND_REMOVE);
            stopSelf();
            System.exit(0);
        }));
    }

    @Override
    public void onDestroy() {
        HatterCallAudio.setListener(null);
        HatterNotifications.setOngoingListener(null);
        HatterNotifications.setStopListener(null);
        super.onDestroy();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        foreground(false);
        return START_STICKY;
    }

    private void foreground(boolean calling) {
        NotificationManager manager = getSystemService(NotificationManager.class);
        manager.createNotificationChannel(new NotificationChannel(
            CHANNEL, getString(R.string.connection_channel), NotificationManager.IMPORTANCE_LOW));
        PendingIntent open = PendingIntent.getActivity(
            this, 0, new Intent(this, MainActivity.class), PendingIntent.FLAG_IMMUTABLE);
        // The call going on, which Android shows only for a foreground
        // service, or else that hTox runs.
        Notification call = HatterNotifications.ongoingCall(this, CHANNEL, R.drawable.ic_call, open);
        Notification notification = call != null ? call : new Notification.Builder(this, CHANNEL)
            .setSmallIcon(R.drawable.ic_chat_bubble_outline)
            .setContentTitle(getString(R.string.app_name))
            .setContentText(HatterNotifications.status().isEmpty()
                ? getString(R.string.connection_running) : HatterNotifications.status())
            .setContentIntent(open)
            .setOngoing(true)
            // Its own group, so Android doesn't bundle it with the messages.
            .setGroup(CHANNEL)
            .build();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
            int types = ServiceInfo.FOREGROUND_SERVICE_TYPE_REMOTE_MESSAGING;
            if (calling) {
                types |= ServiceInfo.FOREGROUND_SERVICE_TYPE_MICROPHONE;
                // Allowed only once the user let us film.
                if (checkSelfPermission(android.Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED) {
                    types |= ServiceInfo.FOREGROUND_SERVICE_TYPE_CAMERA;
                }
            }
            try {
                startForeground(NOTIFICATION, notification, types);
            } catch (SecurityException e) {
                // Without the microphone permission the call has no audio to keep.
                startForeground(NOTIFICATION, notification,
                    ServiceInfo.FOREGROUND_SERVICE_TYPE_REMOTE_MESSAGING);
            }
        } else {
            startForeground(NOTIFICATION, notification);
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
