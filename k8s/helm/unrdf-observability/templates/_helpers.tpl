{{/*
Generate fullname from release name
*/}}
{{- define "unrdf-observability.fullname" -}}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "unrdf-observability.labels" -}}
helm.sh/chart: {{ .Chart.Name }}-{{ .Chart.Version }}
{{ include "unrdf-observability.selectorLabels" . }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "unrdf-observability.selectorLabels" -}}
app.kubernetes.io/name: {{ .Chart.Name }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Component labels (add component name)
*/}}
{{- define "unrdf-observability.componentLabels" -}}
{{ include "unrdf-observability.labels" . }}
app.kubernetes.io/component: {{ .component }}
{{- end }}

{{/*
Service DNS name for inter-service communication
*/}}
{{- define "unrdf-observability.service" -}}
{{- printf "%s-%s" (include "unrdf-observability.fullname" .) .component }}
{{- end }}

{{/*
K8s-internal hostname (service.namespace.svc.cluster.local)
*/}}
{{- define "unrdf-observability.hostname" -}}
{{- printf "%s-%s.%s.svc.cluster.local" (include "unrdf-observability.fullname" .) .component .Release.Namespace }}
{{- end }}

{{/*
Container image helper
*/}}
{{- define "unrdf-observability.image" -}}
{{- printf "%s:%s" .imageRepo .imageTag | quote }}
{{- end }}
