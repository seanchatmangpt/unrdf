{{/*
Expand the name of the chart.
*/}}
{{- define "knowd.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "knowd.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "knowd.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "knowd.labels" -}}
helm.sh/chart: {{ include "knowd.chart" . }}
{{ include "knowd.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- with .Values.commonLabels }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "knowd.selectorLabels" -}}
app.kubernetes.io/name: {{ include "knowd.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- with .Values.commonLabels }}
{{ toYaml . }}
{{- end }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "knowd.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "knowd.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Return the proper knowd image name
*/}}
{{- define "knowd.image" -}}
{{- printf "%s/%s:%s" .Values.image.registry .Values.image.repository (.Values.image.tag | default .Chart.AppVersion) }}
{{- end }}

{{/*
Create a default fully qualified prometheus server name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
*/}}
{{- define "knowd.prometheus.server.fullname" -}}
{{- printf "%s-prometheus-server" (include "knowd.fullname" .) | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified jaeger collector name.
*/}}
{{- define "knowd.jaeger.collector.fullname" -}}
{{- printf "%s-jaeger-collector" (include "knowd.fullname" .) | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Generate full DNS name for knowd service
*/}}
{{- define "knowd.serviceDnsName" -}}
{{- printf "%s.%s.svc.cluster.local" (include "knowd.fullname" .) .Release.Namespace }}
{{- end }}
