package disk

import (
	"context"
	"fmt"
	"io"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"google.golang.org/api/option"
	"google.golang.org/api/storage/v1"
)

// ObjStore manages remote object storage for snapshots.
type ObjStore struct {
	config    ObjStoreConfig
	s3Client  *s3.S3
	gcsClient *storage.Service
}

// ObjStoreConfig configures object store settings.
type ObjStoreConfig struct {
	S3Config  *S3Config  `json:"s3,omitempty"`
	GCSConfig *GCSConfig `json:"gcs,omitempty"`
}

// S3Config configures S3 object storage.
type S3Config struct {
	Bucket    string `json:"bucket"`
	Region    string `json:"region"`
	AccessKey string `json:"access_key,omitempty"`
	SecretKey string `json:"secret_key,omitempty"`
	Endpoint  string `json:"endpoint,omitempty"` // For MinIO or custom S3
}

// GCSConfig configures Google Cloud Storage.
type GCSConfig struct {
	Bucket          string `json:"bucket"`
	ProjectID       string `json:"project_id"`
	CredentialsFile string `json:"credentials_file,omitempty"`
}

// SnapshotMetadata represents metadata for stored snapshots.
type SnapshotMetadata struct {
	ID          string    `json:"id"`
	Size        int64     `json:"size"`
	CreatedAt   time.Time `json:"created_at"`
	Hash        string    `json:"hash"`
	Compression string    `json:"compression"`
}

// NewObjStore creates a new object store with the given configuration.
func NewObjStore(config ObjStoreConfig) (*ObjStore, error) {
	store := &ObjStore{config: config}

	// Initialize S3 client if configured
	if config.S3Config != nil {
		if err := store.initS3Client(); err != nil {
			return nil, fmt.Errorf("failed to initialize S3 client: %w", err)
		}
	}

	// Initialize GCS client if configured
	if config.GCSConfig != nil {
		if err := store.initGCSClient(); err != nil {
			return nil, fmt.Errorf("failed to initialize GCS client: %w", err)
		}
	}

	return store, nil
}

// initS3Client initializes the S3 client.
func (o *ObjStore) initS3Client() error {
	config := &aws.Config{
		Region: aws.String(o.config.S3Config.Region),
	}

	if o.config.S3Config.Endpoint != "" {
		config.Endpoint = aws.String(o.config.S3Config.Endpoint)
		config.S3ForcePathStyle = aws.Bool(true)
	}

	if o.config.S3Config.AccessKey != "" && o.config.S3Config.SecretKey != "" {
		config.Credentials = credentials.NewStaticCredentials(
			o.config.S3Config.AccessKey,
			o.config.S3Config.SecretKey,
			"",
		)
	}

	sess, err := session.NewSession(config)
	if err != nil {
		return err
	}

	o.s3Client = s3.New(sess)
	return nil
}

// initGCSClient initializes the GCS client.
func (o *ObjStore) initGCSClient() error {
	ctx := context.Background()

	var client *storage.Service
	var err error

	if o.config.GCSConfig.CredentialsFile != "" {
		client, err = storage.NewService(ctx, option.WithCredentialsFile(o.config.GCSConfig.CredentialsFile))
	} else {
		// Use default credentials
		client, err = storage.NewService(ctx)
	}

	if err != nil {
		return err
	}

	o.gcsClient = client
	return nil
}

// PushSnapshot uploads a snapshot to remote storage.
func (o *ObjStore) PushSnapshot(ctx context.Context, snapshotPath string, remotePath string) (*SnapshotMetadata, error) {
	// Read the snapshot file
	file, err := os.Open(snapshotPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open snapshot file: %w", err)
	}
	defer file.Close()

	fileInfo, err := file.Stat()
	if err != nil {
		return nil, fmt.Errorf("failed to stat snapshot file: %w", err)
	}

	// Determine storage provider from URL scheme
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return nil, fmt.Errorf("invalid remote path: %w", err)
	}

	switch parsedURL.Scheme {
	case "s3":
		return o.pushToS3(ctx, file, remotePath, fileInfo.Size())
	case "gs":
		return o.pushToGCS(ctx, file, remotePath, fileInfo.Size())
	default:
		return nil, fmt.Errorf("unsupported storage scheme: %s", parsedURL.Scheme)
	}
}

// PullSnapshot downloads a snapshot from remote storage.
func (o *ObjStore) PullSnapshot(ctx context.Context, remotePath string, localPath string) error {
	// Create local directory if needed
	dir := filepath.Dir(localPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create local directory: %w", err)
	}

	// Determine storage provider from URL scheme
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return fmt.Errorf("invalid remote path: %w", err)
	}

	switch parsedURL.Scheme {
	case "s3":
		return o.pullFromS3(ctx, remotePath, localPath)
	case "gs":
		return o.pullFromGCS(ctx, remotePath, localPath)
	default:
		return fmt.Errorf("unsupported storage scheme: %s", parsedURL.Scheme)
	}
}

// pushToS3 uploads a file to S3.
func (o *ObjStore) pushToS3(ctx context.Context, reader io.Reader, remotePath string, size int64) (*SnapshotMetadata, error) {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return nil, err
	}

	key := strings.TrimPrefix(parsedURL.Path, "/")

	uploader := s3manager.NewUploaderWithClient(o.s3Client)
	_, err = uploader.UploadWithContext(ctx, &s3manager.UploadInput{
		Bucket: aws.String(o.config.S3Config.Bucket),
		Key:    aws.String(key),
		Body:   reader,
	})

	if err != nil {
		return nil, fmt.Errorf("failed to upload to S3: %w", err)
	}

	return &SnapshotMetadata{
		ID:        key,
		Size:      size,
		CreatedAt: time.Now(),
		Hash:      "", // Could compute hash if needed
	}, nil
}

// pullFromS3 downloads a file from S3.
func (o *ObjStore) pullFromS3(ctx context.Context, remotePath string, localPath string) error {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return err
	}

	key := strings.TrimPrefix(parsedURL.Path, "/")

	downloader := s3manager.NewDownloaderWithClient(o.s3Client)
	file, err := os.Create(localPath)
	if err != nil {
		return fmt.Errorf("failed to create local file: %w", err)
	}
	defer file.Close()

	_, err = downloader.DownloadWithContext(ctx, file, &s3.GetObjectInput{
		Bucket: aws.String(o.config.S3Config.Bucket),
		Key:    aws.String(key),
	})

	if err != nil {
		return fmt.Errorf("failed to download from S3: %w", err)
	}

	return nil
}

// pushToGCS uploads a file to Google Cloud Storage.
func (o *ObjStore) pushToGCS(ctx context.Context, reader io.Reader, remotePath string, size int64) (*SnapshotMetadata, error) {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return nil, err
	}

	objectName := strings.TrimPrefix(parsedURL.Path, "/")

	// Create the object
	object := &storage.Object{
		Name: objectName,
	}

	// Upload the file
	_, err = o.gcsClient.Objects.Insert(o.config.GCSConfig.Bucket, object).Media(reader).Do()
	if err != nil {
		return nil, fmt.Errorf("failed to upload to GCS: %w", err)
	}

	return &SnapshotMetadata{
		ID:        objectName,
		Size:      size,
		CreatedAt: time.Now(),
		Hash:      "", // Could compute hash if needed
	}, nil
}

// pullFromGCS downloads a file from Google Cloud Storage.
func (o *ObjStore) pullFromGCS(ctx context.Context, remotePath string, localPath string) error {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return err
	}

	objectName := strings.TrimPrefix(parsedURL.Path, "/")

	// Get the object
	resp, err := o.gcsClient.Objects.Get(o.config.GCSConfig.Bucket, objectName).Download()
	if err != nil {
		return fmt.Errorf("failed to download from GCS: %w", err)
	}
	defer resp.Body.Close()

	// Create local file
	file, err := os.Create(localPath)
	if err != nil {
		return fmt.Errorf("failed to create local file: %w", err)
	}
	defer file.Close()

	// Copy data
	_, err = io.Copy(file, resp.Body)
	if err != nil {
		return fmt.Errorf("failed to write to local file: %w", err)
	}

	return nil
}

// ListSnapshots lists snapshots in remote storage.
func (o *ObjStore) ListSnapshots(ctx context.Context, prefix string) ([]SnapshotMetadata, error) {
	// For simplicity, return empty list for now
	// In a full implementation, you'd list objects from S3/GCS
	return []SnapshotMetadata{}, nil
}

// DeleteSnapshot deletes a snapshot from remote storage.
func (o *ObjStore) DeleteSnapshot(ctx context.Context, remotePath string) error {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return fmt.Errorf("invalid remote path: %w", err)
	}

	switch parsedURL.Scheme {
	case "s3":
		return o.deleteFromS3(ctx, remotePath)
	case "gs":
		return o.deleteFromGCS(ctx, remotePath)
	default:
		return fmt.Errorf("unsupported storage scheme: %s", parsedURL.Scheme)
	}
}

// deleteFromS3 deletes an object from S3.
func (o *ObjStore) deleteFromS3(ctx context.Context, remotePath string) error {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return err
	}

	key := strings.TrimPrefix(parsedURL.Path, "/")

	_, err = o.s3Client.DeleteObjectWithContext(ctx, &s3.DeleteObjectInput{
		Bucket: aws.String(o.config.S3Config.Bucket),
		Key:    aws.String(key),
	})

	if err != nil {
		return fmt.Errorf("failed to delete from S3: %w", err)
	}

	return nil
}

// deleteFromGCS deletes an object from GCS.
func (o *ObjStore) deleteFromGCS(ctx context.Context, remotePath string) error {
	parsedURL, err := url.Parse(remotePath)
	if err != nil {
		return err
	}

	objectName := strings.TrimPrefix(parsedURL.Path, "/")

	err = o.gcsClient.Objects.Delete(o.config.GCSConfig.Bucket, objectName).Do()
	if err != nil {
		return fmt.Errorf("failed to delete from GCS: %w", err)
	}

	return nil
}

// GetConfig returns the object store configuration.
func (o *ObjStore) GetConfig() ObjStoreConfig {
	return o.config
}

// Close closes the object store and releases resources.
func (o *ObjStore) Close() error {
	// Clean up connections if needed
	return nil
}
